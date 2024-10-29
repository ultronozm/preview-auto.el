;;; preview-auto.el --- Automatic previews in AUCTeX     -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.4
;; URL: https://github.com/ultronozm/preview-auto.el
;; Package-Requires: ((emacs "29.3") (auctex "14.0.5"))
;; Keywords: tex, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a minor mode `preview-auto-mode' in which the
;; visible portion of an AUCTeX buffer is continuously previewed.  It
;; can be toggled via M-x preview-auto-mode, C-c C-p C-a, or the
;; Preview menu.
;;
;; This package works best with the following AUCTeX settings:
;;
;; (setq preview-protect-point t)
;; (setq preview-locating-previews-message nil)
;; (setq preview-leave-open-previews-visible t)
;; (setq preview-LaTeX-command-replacements '(preview-LaTeX-disable-pdfoutput))
;;
;; See README.org for further details.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'latex)
(require 'preview)

(defgroup preview-auto nil
  "Settings for preview-auto."
  :group 'preview)

(defcustom preview-auto-interval 0.1
  "Interval for preview timer.
For this to have any effect, it must be set before
`preview-auto-mode' is activated for the first time."
  :type 'number)

(defcustom preview-auto-chars-above 5000
  "Controls how many characters above point to preview."
  :type 'integer)

(defcustom preview-auto-chars-below 6000
  "Controls how many characters below point to preview."
  :type 'integer)

(defvar preview-auto--timer nil)

(defvar-local preview-auto--keepalive t
  "Used to keep track of when we should preview some more.")

(defcustom preview-auto--extra-environments nil
  "Extra environments to consider for automatic previewing."
  :type '(repeat string))

(defcustom preview-auto-detect-math-via-font-lock '(tex-mode)
  "List of modes in which to use font-lock to detect math environments.
Applies also to any derived modes.  This is faster than using
`texmathp', but may not work outside tex major modes without additional
setup.  Set to nil to always use `texmathp' (e.g., if you work in
`LaTeX-mode' with font-lock disabled)."
  :type '(repeat symbol))

(defun preview-auto--math-p ()
  "Return non-nil if point is in a math environment.
Should work in AUCTeX `LaTeX-mode' buffers.  Implemented using
`font-latex-math-face'."
  (if (seq-some #'derived-mode-p preview-auto-detect-math-via-font-lock)
      (let ((math-face 'font-latex-math-face)
            (face (plist-get (text-properties-at (point))
                             'face)))
        (or (eq face math-face)
            (and (listp face)
                 (memq math-face face))))
    (texmathp)))

(defun preview-auto--generate-rules ()
  "Return list of rules for identifying math environments."
  (let* ((basic-rules
          (mapcar (lambda (pair)
                    (cons (car pair)
                          (cons (cdr pair)
                                (if (member (car pair) '("$" "$$"))
                                    '(preview-auto--math-p)
                                  t))))
                  '(("$" . "$") ("$$" . "$$") ("\\(" . "\\)") ("\\[" . "\\]"))))
         (env-rules
          (mapcar (lambda (env)
                    (cons (format "\\begin{%s}" env)
                          (cons (format "\\end{%s}" env) t)))
                  (append texmathp-environments
                          preview-auto--extra-environments)))
         (rules (append basic-rules env-rules)))
    rules))

(defvar-local preview-auto--rules nil
  "Rules for identifying math environments.
Each rule is an iterated cons cell ((BEGIN . END) . PREDICATE), where
BEGIN and END are the delimiters and PREDICATE is a Lisp form, evaluated
just beyond the BEGIN delimiter, that returns non-nil if the environment
is valid.")

(defvar-local preview-auto--begin-re nil
  "Regular expression for identifying the beginning of a math environment.")

(defun preview-auto--check-default ()
  "Default predicate for checking whether to consider a delimiter.
Returns non-nil if either

- we're in a tex mode and it is NOT in a comment,

- we're in a programming mode it IS in a comment, or

- we are in some other mode (e.g., a non-tex text mode)."
  (if (derived-mode-p 'tex-mode)
      (not (TeX-in-comment))
    (if (derived-mode-p 'prog-mode)
        (nth 4 (syntax-ppss))
      t)))

(defcustom preview-auto-check-function #'preview-auto--check-default
  "Predicate for checking whether to consider a delimiter."
  :type 'function)

(defun preview-auto--search (regexp bound)
  "Search for REGEXP before BOUND.
Ignore anything for which the call to `preview-auto-check-function'
returns nil."
  (catch 'found
    (while (re-search-forward regexp bound t)
      (when (funcall preview-auto-check-function)
        (throw 'found (point))))))

(defcustom preview-auto-refresh-after-compilation t
  "If non-nil, refresh previews after each compilation.
This plays well with the packages `auctex-label-numbers' and
`auctex-cont-latexmk': the result is that preview equation numbers are
updated automatically to the correct form."
  :type 'boolean)

(defconst preview-auto--refresh-delay '(0 1)
  "Delay, as a time value, before refreshing previews after compilation.
This is used to avoid refreshing previews while the aux file is in some
intermediate state.")

(defun preview-auto--tex-fold-at (&optional pos)
  "Return non-nil when there is a tex-fold at POS."
  (seq-some
   (lambda (ov)
     (eq (overlay-get ov 'category) 'TeX-fold))
   (overlays-at (or pos (point)))))

(defun preview-auto--already-previewed-at (&optional pos)
  "Return non-nil when there is a non-disabled preview overlay at POS.
A preview is considered non-disabled if it is active or inactive
according to `preview.el'.  If `preview-auto-refresh-after-compilation'
is non-nil, then we further require that the preview has been generated
more recently than the aux file.  This last requirement, when combined
with the `auctex-label-numbers' and `auctex-cont-latexmk' packages,
ensures that preview equation numbers are updated automatically."
  (seq-some
   (lambda (ov)
     (and
      (memq (overlay-get ov 'preview-state) '(active inactive))
      (or (null preview-auto-refresh-after-compilation)
          (let* ((image (overlay-get ov 'preview-image))
                 (image-file (nth 1 image))
                 (image-time (nth 5 (file-attributes image-file)))
                 (aux-file (TeX-master-output-file "aux"))
                 (aux-time (nth 5 (file-attributes aux-file))))
            (or (null image-time)
                (null aux-time)
                (time-less-p aux-time image-time)
                (time-less-p (current-time)
                             (time-add aux-time
                                       preview-auto--refresh-delay)))))))
   (overlays-at (or pos (point)))))

(defcustom preview-auto-predicate nil
  "Additional predicate for determining preview validity.
See the documentation of `preview-auto--next-env' for details."
  :type 'function)

(defun preview-auto--allow-at (&optional pos)
  "Return non-nil if previewing should happen at POS."
  (and (not (preview-auto--tex-fold-at pos))
       (not (preview-auto--already-previewed-at pos))
       (or (null preview-auto-predicate)
           (funcall preview-auto-predicate))))

(defun preview-auto--truncated-bound (bound)
  "Return last position before BOUND and any blank lines."
  (save-excursion
    (if (re-search-forward "[\n\r][[:space:]]*[\n\r]" bound t)
        (match-beginning 0)
      bound)))

(defun preview-auto--next-env (bound)
  "Find next LaTeX math environment before BOUND.
Return iterated cons cell ((BEGIN . END) . VALIDITY) describing bounds
for the environment and whether it is considered valid for preview,
which means that (1) it consists of more than just whitespace, (2) it
has not already been previewed, and (3) the customizable predicate
`preview-auto-predicate' holds."
  (catch 'found
    (while (preview-auto--search preview-auto--begin-re bound)
      (let* ((begin (match-beginning 0))
             (inner-begin (match-end 0))
             (begin-string (match-string 0))
             (rule (cdr (assoc begin-string preview-auto--rules)))
             (end-string (car rule))
             (pred (cdr rule)))
        (when (eval pred)
          (when-let*
              ((limit (preview-auto--truncated-bound bound))
               (end (preview-auto--search (regexp-quote end-string) limit))
               (inner-end (- end (length end-string))))
            (let ((validity (and
                             (string-match-p "[^[:space:]\n\r]"
                                             (buffer-substring-no-properties
                                              inner-begin inner-end))
                             (preview-auto--allow-at begin))))
              (throw 'found (cons (cons begin end) validity)))))))))

(defun preview-auto--get-streaks (lst)
  "Return a list describing the non-nil streaks in list LST.
A streak is a maximal collection of consecutive non-nil values.  We
describe a streak by a cons cell containing its first and last indices.

Example: (nil t t t nil nil t nil) => ((1 . 3) (6 . 6))"
  (let ((intervals nil)
        (start nil)
        (end nil))
    (dotimes (i (length lst))
      (if (nth i lst)
          (progn (when (not start)
                   (setq start i))
                 (setq end i))
        (when (and start end)
          (push (cons start end) intervals)
          (setq start nil)
          (setq end nil))))
    (when (and start end)
      (push (cons start end) intervals))
    (nreverse intervals)))

(defcustom preview-auto-barriers
  '("%"
    "\\\\begin{abstract}"
    "\\\\includegraphics"
    "\\\\part"
    "\\\\chapter"
    "\\\\section"
    "\\\\subsection"
    "\\\\subsubsection"
    "\\\\paragraph"
    "\\\\subparagraph")
  "List of barrier regexps, excluded from in regions sent for previewing."
  :type '(repeat string))

(defun preview-auto--get-valid-region (beg end search-from-beginning)
  "Return a maximal valid region for previewing between BEG and END.
The search starts from BEG if SEARCH-FROM-BEGINNING is non-nil, and
otherwise from END."
  (cl-assert (<= end (+ beg preview-auto-chars-above preview-auto-chars-below)))
  (when (<= beg end)
    (let (envs)
      (save-excursion
        (goto-char beg)
        (while-let ((env (preview-auto--next-env end)))
          (push env envs))
        (setq envs (nreverse envs)))
      (when-let* ((streaks (preview-auto--get-streaks
                            (mapcar #'cdr envs))))
        (let* ((closest (if search-from-beginning
                            (car streaks)
                          (car (last streaks)))))
          ;; Shrink the region so that it doesn't cross any barriers.
          (when-let
              ((shortening
                (if search-from-beginning
                    (seq-some
                     (lambda (i)
                       (when (seq-some
                              (lambda (re)
                                (save-excursion
                                  (goto-char (cdar (nth i envs)))
                                  (preview-auto--search
                                   re (caar (nth (1+ i) envs)))))
                              preview-auto-barriers)
                         (cons (car closest) i)))
                     (number-sequence (car closest) (1- (cdr closest))))
                  (seq-some
                   (lambda (j)
                     (when (seq-some
                            (lambda (re)
                              (save-excursion
                                (goto-char (cdar (nth (1- j) envs)))
                                (preview-auto--search
                                 re (caar (nth j envs)))))
                            preview-auto-barriers)
                       (cons j (cdr closest))))
                   (number-sequence (cdr closest) (1+ (car closest)) -1)))))
            (setq closest shortening))
          (cons (caar (nth (car closest) envs))
                (cdar (nth (cdr closest) envs))))))))

(defun preview-auto--first-valid-region (beg end)
  "Return first maximal valid region for previewing between BEG and END."
  (preview-auto--get-valid-region beg end t))

(defun preview-auto--last-valid-region (beg end)
  "Return last maximal valid region for previewing between BEG and END."
  (preview-auto--get-valid-region beg end nil))

(defvar preview-auto--debug nil
  "If non-nil, print debug messages.")

(defun preview-auto--debug-log (format-string &rest args)
  "Print a debug message if `preview-auto--debug' is non-nil.
FORMAT-STRING is a format string, and ARGS are its arguments."
  (when preview-auto--debug
    (with-current-buffer (get-buffer-create "*preview-auto-debug*")
      (goto-char (point-min))
      (insert
       (format-time-string "%Y-%m-%d %H:%M:%S.%6N ")
       (apply #'format format-string args))
      (insert "\n"))))

(defun preview-auto--silent-write-region (orig-fun &rest args)
  "Like `write-region', but suppresses messages.
Imperfection: still causes current message to disappear.

ORIG-FUN is the original function, and ARGS are its arguments."
  (let ((noninteractive t)
        (inhibit-message t)
        message-log-max)
    (apply orig-fun args)))

(defun preview-auto--region-wrapper (beg end)
  "Preview region between BEG and END, possibly inhibiting messages."
  (preview-auto--debug-log "Previewing region %d,  %d" beg end)
  (let ((TeX-suppress-compilation-message t)
        (save-silently t))
    (advice-add 'write-region :around #'preview-auto--silent-write-region)
    (prog1
        ;; If we are working in a file buffer that is not a tex file,
        ;; then we want preview-region to operate in "non-file" mode,
        ;; where it passes "<none>" to TeX-region-create.
        (if (eq TeX-master t)
            (preview-region beg end)
          (let ((buffer-file-name nil))
            (preview-region beg end)))
      (advice-remove 'write-region #'preview-auto--silent-write-region))))

(defun preview-auto--update-editing-region ()
  "Update preview of environment being edited."
  (when (texmathp)
    (let ((why (car texmathp-why))
          (begin (cdr texmathp-why)))
      (when (preview-auto--allow-at begin)
        (unless (member why '("$" "$$" "\\(" "\\["))
          (setq why (format "\\begin{%s}" why)))
        (let ((limit (save-excursion
                       (goto-char begin)
                       (preview-auto--truncated-bound (point-max)))))
          (when (> limit (point))
            (when-let*
                ((end-string (cadr (assoc why preview-auto--rules)))
                 (end (save-excursion
                        (preview-auto--search (regexp-quote end-string)
                                              limit))))
              ;; Don't preview essentially empty regions.  Exclude
              ;; backslashes to avoid issues with previewing
              ;; regions-in-progress such as "$\$".
              (when (string-match-p "[^[:space:]\n\r\\]"
                                    (buffer-substring-no-properties
                                     (+ begin (length why))
                                     (- end (length end-string))))
                ;; Avoid error-prone updates for multi-line $...$.
                (unless
                    (and (string= why "$")
                         (string-match
                          "[\n\r]" (buffer-substring-no-properties begin end)))
                  (preview-auto--debug-log "Previewing editing region")
                  (preview-auto--region-wrapper begin end))))))))))

(defun preview-auto--base-range ()
  "Return the base range for previewing.
This is a window around point controlled by the user options
`preview-auto-chars-below' and `preview-auto-chars-above', as well as
the beginning and end of the document."
  (let* ((begin-document
          (or (and (derived-mode-p 'tex-mode)
                   (save-excursion
                     (goto-char (point-min))
                     (when (re-search-forward TeX-header-end nil t)
                       (match-end 0))))
              (point-min)))
         (end-document
          (or (and (derived-mode-p 'tex-mode)
                   (save-excursion
                     (goto-char (point-min))
                     (when (re-search-forward TeX-trailer-start nil t)
                       (match-beginning 0))))
              (point-max)))
         (pmin (max begin-document
                    (- (point) preview-auto-chars-above)))
         (pmax (min end-document
                    (+ (point) preview-auto-chars-below))))
    (cons pmin pmax)))

(defun preview-auto--preview-something ()
  "Run `preview-region' on an appropriate region.
Identify top level math environments near the window.  Find a contiguous
group of regions at which there are no active or inactive previews at
point.  Call `preview-region' on the smallest region that contains this
group."
  (unless (or (get-buffer-process (TeX-process-buffer-name (TeX-region-file)))
              (get-buffer-process (TeX-process-buffer-name (TeX-master-file))))
    (setq preview-auto--rules (preview-auto--generate-rules))
    (setq preview-auto--begin-re
          (regexp-opt (mapcar #'car preview-auto--rules) t))
    (pcase-let ((`(,pmin . ,pmax) (preview-auto--base-range)))
      (setq preview-auto--keepalive t)
      (cond
       ((and
         (< pmin (point) pmax)
         preview-protect-point
         (preview-auto--update-editing-region)))
       ((let ((region-above (preview-auto--last-valid-region
                             pmin (min pmax (point))))
              (region-below (preview-auto--first-valid-region
                             (max pmin (point)) pmax)))
          (when (or region-above region-below)
            (let* ((should-preview-above
                    (or (not region-below)
                        (and region-above region-below
                             (<= (- (point) (cdr region-above))
                                 (- (car region-below) (point))))))
                   (region (if should-preview-above region-above region-below)))
              (preview-auto--debug-log
               (concat "Previewing "
                       (if should-preview-above "above" "below")
                       (when (and region-above region-below)
                         "(closer)")))
              (prog1 t
                (preview-auto--region-wrapper (car region) (cdr region)))))))
       (t
        (setq preview-auto--keepalive nil))))))

(defvar preview-auto-mode)

(defun preview-auto--timer-function ()
  "Function called by the preview timer to update LaTeX previews."
  (and preview-auto-mode
       preview-auto--timer
       preview-auto--keepalive
       (preview-auto--preview-something)))

(defun preview-auto-conditionally-enable ()
  "Enable `preview-auto-mode' if appropriate.
Check that we are not visiting a bbl file."
  (unless (and (buffer-file-name)
               (string-match-p "\\.bbl\\'" (buffer-file-name)))
    (preview-auto-mode 1)))

(defun preview-auto--after-change (beg end length)
  "Hook function for `preview-auto-mode'.
BEG is the start of the modified region, END is the end of the region,
and LENGTH is the length of the modification.  If the modification
occurs before some region where a preview is being generated, then
cancel the preview, so that the preview is not misplaced."
  (save-match-data
    (preview-auto--debug-log "After change:")
    (preview-auto--debug-log "  %d, %d, %d" beg end length)
    (when preview-current-region
      (preview-auto--debug-log "  (%d, %d)"
                               (car preview-current-region)
                               (cdr preview-current-region))
      (when-let ((proc (get-buffer-process
                        (TeX-process-buffer-name (TeX-region-file)))))
        (preview-auto--debug-log "  region: %s" proc))
      (when-let ((proc (get-buffer-process
                        (TeX-process-buffer-name (TeX-master-file)))))
        (preview-auto--debug-log "  master: %s" proc)))
    (if (and preview-current-region
             (< beg (cdr preview-current-region)))
        (progn
          (preview-auto--debug-log "Cancelling preview")
          (ignore-errors (TeX-kill-job))
          (setq preview-abort-flag t))
      (preview-auto--debug-log "Not cancelling preview"))))

(defun preview-auto--post-command ()
  "Function called after each command in `preview-auto-mode'."
  (setq preview-auto--keepalive t))

(defvar preview-auto-mode)

;;;###autoload
(define-minor-mode preview-auto-mode
  "Minor mode for running LaTeX preview on a timer."
  :lighter nil
  (cond
   (preview-auto-mode
    (unless (or (derived-mode-p 'LaTeX-mode)
                (and
                 (stringp TeX-master)
                 (file-exists-p TeX-master)))
      (setq preview-auto-mode nil)
      (user-error "Not in a LaTeX buffer, and TeX-master not set to a file"))
    (unless TeX-header-end
      (setq TeX-header-end LaTeX-header-end))
    (unless TeX-trailer-start
      (setq TeX-trailer-start LaTeX-trailer-start))
    (unless comment-start-skip ; for texmathp
      (setq comment-start-skip
            (concat "\\(\\(^\\|[^\\\n]\\)\\("
                    (regexp-quote TeX-esc)
                    (regexp-quote TeX-esc)
                    "\\)*\\)\\(%+[ \t]*\\)")))
    (unless (memq #'preview-move-point post-command-hook)
      (add-hook 'post-command-hook #'preview-move-point nil t))
    (add-hook 'after-change-functions #'preview-auto--after-change nil t)
    (add-hook 'post-command-hook #'preview-auto--post-command nil t)
    (when preview-auto--timer
      ;; Reset the timer, in case it's broken.
      (cancel-timer preview-auto--timer)
      (setq preview-auto--timer nil))
    (setq preview-auto--timer (run-with-timer preview-auto-interval
                                              preview-auto-interval
                                              #'preview-auto--timer-function))
    (setq preview-auto--keepalive t))
   (t
    (remove-hook 'after-change-functions #'preview-auto--after-change t)
    (remove-hook 'post-command-hook #'preview-auto--post-command t))))

;;;###autoload
(defun preview-auto-setup ()
  "Set up keybinding and menu item for `preview-auto-mode'.
This should be run after AUCTeX has loaded to a sufficient extent,
e.g., in `LaTeX-mode-hook'."
  (remove-hook 'LaTeX-mode-hook #'preview-auto-setup)
  (define-key LaTeX-mode-map (kbd "C-c C-p C-a") #'preview-auto-mode)
  (easy-menu-add-item
   nil '("Preview")
   ["automatically" preview-auto-mode]
   "(or toggle) at point"))

(provide 'preview-auto)
;;; preview-auto.el ends here
