;;; preview-auto.el --- automatic latex previews     -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
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

;; This package provides a minor mode `preview-auto-mode' that
;; automatically previews the visible portion of an AUCTeX buffer.

;;; Code:

(require 'cl-lib)
(require 'latex)
(require 'preview)

(defgroup preview-auto nil
  "Settings for preview-auto."
  :group 'preview
  :prefix "preview-auto-")

(defcustom preview-auto-timer-interval 0.3
  "Interval for preview timer.
For this to have any effect, it must be set before
`preview-auto-mode' is activated for the first time."
  :type 'number)

(defcustom preview-auto-chars-above 5000
  "Controls how many characters above point to preview."
  :type 'integer)

(defcustom preview-auto-chars-below 10000
  "Controls how many characters below point to preview."
  :type 'integer)

(defvar preview-auto--timer nil)

(defvar-local preview-auto--timer-enabled nil
  "Is the preview timer is enabled in this buffer?
We want the preview timer to be active only in the current buffer.  For
this reason, it is a global object.  This local variable keeps track of
the buffers in which the timer should do anything.")

(defvar-local preview-auto--keepalive t
  "Used to keep track of when we should preview some more.")

(defvar preview-auto--editing nil)

;; (setq preview-leave-open-previews-visible t)

(defcustom preview-auto-)

(defun preview-auto--generate-rules ()
  "Return list of rules for identifying math environments.
Each rule is an iterated cons cell ((BEGIN . END) . PREDICATE), where
BEGIN and END are the delimiters and PREDICATE is a function, called
just beyond the BEGIN delimiter, that returns non-nil if the environment
is valid."
  (let* ((basic-rules
          (mapcar (lambda (pair)
                    (cons (car pair)
                          (cons (cdr pair) '(texmathp))))
                  '(("$" . "$") ("$$" . "$$") ("\\(" . "\\)") ("\\[" . "\\]"))))
         (env-rules
          (mapcar (lambda (env)
                    (cons (format "\\begin{%s}" env)
                          (cons (format "\\end{%s}" env) t)))
                  texmathp-environments))
         (rules (append basic-rules env-rules)))
    rules))

(defun preview-auto--search (regexp bound)
  "Search for REGEXP before BOUND.
Ignore comments and verbatim environments."
  (catch 'found
    (while (re-search-forward regexp bound t)
      (when (and (not (TeX-in-comment))
                 (not (LaTeX-verbatim-p)))
        (throw 'found (point))))))

(defun preview-auto--already-previewed-at (pos)
  "Return non-nil when there a non-disabled preview overlay at POS."
  (cl-intersection
   (mapcar
    (lambda (ov) (overlay-get ov 'preview-state))
    (overlays-at (or pos (point))))
   '(active inactive)))

(defcustom preview-auto-predicate nil
  "Additional predicate for determining preview validity.
See the documentation of `preview-auto--next-env' for details."
  :type 'function
  :group 'preview-auto)

(defun preview-auto--next-env (bound rules)
  "Find next LaTeX math environment before BOUND using RULES.
Return list of iterated cons cells ((BEGIN . END) . VALIDITY) describing
the bounds for the environment and whether it is considered valid for
preview, which means that (1) it consists of more than just
whitespace, (2) it has not already been previewed, and (3) the
customizable predicate `preview-auto-predicate' holds."
  (interactive)
  (catch 'found
    (let ((begin-regexp (regexp-opt (mapcar #'car rules) t)))
      (while (preview-auto--search begin-regexp bound)
        (let* ((begin (match-beginning 0))
               (inner-begin (match-end 0))
               (begin-string (match-string 0))
               (rule (cdr (assoc begin-string rules)))
               (end-string (car rule))
               (pred (cdr rule)))
          (when (eval pred)
            (when-let*
                ((limit (preview-auto--truncated-bound bound))
                 (end (preview-auto--search (regexp-quote end-string) limit))
                 (inner-end (- end (length end-string)))
                 (validity
                  (and (string-match-p "[^[:space:]\n\r"
                                       (buffer-substring-no-properties
                                        inner-begin inner-end))
                       (not (preview-auto--already-previewed-at begin))
                       (or (null preview-auto-predicate)
                           (funcall preview-auto-predicate)))))
              (throw 'found (cons (cons begin end) validity)))))))))

(defun preview-auto--get-streaks (lst)
  "Return a list describing the non-nil streaks in list LST.
A streak is a maximal collection of consecutive non-nil values.  We
describe a streak by a cons cell containing its first and last indices.
Example: (nil t t nil nil t) => ((1 . 2) (5 . 5)."
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

(defun preview-auto--get-valid-region (beg end direction rules)
  "Return a maximal valid region for previewing between BEG and END.
DIRECTION determines whether to search from the beginning (if non-nil)
or the end (if nil).  RULES is a list of rules for identifying math
environments, as described in `preview-auto--generate-rules'."
  (when (<= beg end)
    (cl-assert (<= end (+ beg preview-auto-chars-above preview-auto-chars-below)))
    (let (envs)
      (save-excursion
        (goto-char beg)
        (while-let ((env (preview-auto--next-env end rules)))
          (push env envs))
        (setq envs (nreverse envs)))
      (when-let* ((streaks (preview-auto--get-streaks
                            (mapcar #'cdr envs)))
                  (closest (if direction
                               (car streaks)
                             (car (last streaks)))))
        (cons (caar (nth (car closest) envs))
              (cdar (nth (cdr closest) envs)))))))

(defun preview-auto--truncated-bound (bound)
  "Return last position before BOUND and any blank lines."
  (save-excursion
    (if (re-search-forward "[\n\r][[:space:]]*[\n\r]" bound t)
        (match-beginning 0)
      bound)))

(defun preview-auto--first-valid-region (beg end rules)
  "Return first maximal valid region for previewing between BEG and END.
Use RULES to identify math environments."
  (preview-auto--get-valid-region beg end t rules))

(defun preview-auto--last-valid-region (beg end rules)
  "Return last maximal valid region for previewing between BEG and END.
Use RULES to identify math environments."
  (preview-auto--get-valid-region beg end nil rules))

(defvar preview-auto--inhibit-message t
  "If non-nil, inhibit messages in `preview-auto--preview-something'.")

(defun preview-auto--preview-something (rules)
  "Run `preview-region' on an appropriate region.
Identify top level math environments near the window using RULES.  Find
the first contiguous group of regions at which there are no active or
inactive previews at point.  Call `preview-region' on the smallest
region that contains this group."
  (interactive)
  (unless (or (get-buffer-process (TeX-process-buffer-name (TeX-region-file)))
              (get-buffer-process (TeX-process-buffer-name (TeX-master-file))))
    (let*
        ((begin-document
          (or (save-excursion
                (goto-char (point-min))
                (when (re-search-forward TeX-header-end nil t)
                  (match-end 0)))
              (point-min)))
         (end-document
          (or (save-excursion
                (goto-char (point-min))
                (when (re-search-forward TeX-trailer-start nil t)
                  (match-beginning 0)))
              (point-max)))
         (pmin (max begin-document
                    (- (point) preview-auto-chars-above)))
         (pmax (min end-document
                    (+ (point) preview-auto-chars-below)))
         (action (lambda (region)
                   (let ((inhibit-message preview-auto--inhibit-message))
                     (preview-region (car region) (cdr region)))))
         region)
      (setq preview-auto--keepalive t)
      (setq preview-auto--editing nil)
      (cond
       ;; Attempt to preview something about point.
       ((setq region (preview-auto--last-valid-region
                      pmin (min end-document (point)) rules))
        (funcall action region))
       ;; Attempt to preview something below point.
       ((setq region (preview-auto--first-valid-region
                      (max begin-document (point)) pmax rules))
        (funcall action region))
       ;; Attempt update of env being edited.
       ((and
         preview-protect-point
         (< begin-document (point) end-document)
         (when (texmathp)
           (let ((why (car texmathp-why))
                 (beg (cdr texmathp-why)))
             (unless (member why '("$" "$$" "\\(" "\\["))
               (setq why (format "\\begin{%s}" why)))
             (unless (preview-auto--already-previewed-at beg)
               (let ((limit (save-excursion
                              (goto-char beg)
                              (preview-auto--truncated-bound (point-max)))))
                 (when (> limit (point))
                   (when-let*
                       ((end-string (cadr (assoc why rules)))
                        (end (save-excursion
                               (preview-auto--search (regexp-quote end-string) limit))))
                     ;; Avoid error-prone updates for multi-line $...$.
                     (unless (and (string= why "$")
                                  (string-match "[\n\r]"
                                                (buffer-substring-no-properties beg end)))
                       (setq preview-auto--editing t)
                       (funcall action (cons beg end)))))))))))
       (t
        (setq preview-auto--keepalive nil))))))

(defun preview-auto--timer-function ()
  "Function called by the preview timer to update LaTeX previews."
  (interactive)
  (and (eq major-mode 'LaTeX-mode)
       preview-auto--timer
       preview-auto--timer-enabled
       preview-auto--keepalive
       (preview-auto--preview-something (preview-auto--generate-rules))))

(defun preview-auto-conditionally-enable ()
  "Enable `preview-auto-mode' if appropriate.
Check that we are not visiting a bbl file."
  (unless (and (buffer-file-name)
               (string-match-p "\\.bbl\\'" (buffer-file-name)))
    (preview-auto-mode 1)))

(defun preview-auto--after-change-function (beg _end _length)
  "Hook function for `preview-auto-mode'.
BEG is the start of the modified region, END is the end of the region,
and LENGTH is the length of the modification.  If the modification
occurs before some region where a preview is being generated, then
cancel the preview, so that the preview is not misplaced."
  (when (and preview-current-region
             (< beg (cdr preview-current-region)))
    (ignore-errors (TeX-kill-job))))

(defun preview-auto--post-command-function ()
  "Function called after each command in `preview-auto-mode'."
  (setq preview-auto--keepalive t))

(defvar preview-auto-mode)

(defun preview-auto--delete-blank-lines-if-editing (str)
  "Remove blank lines from STR if we are editing a preview.
Return the result.  This function is intended to be used as a
`preview-preprocess-function'."
  ;; We check whether `preview-auto-mode' is active because
  ;; `preview-preprocess-functions' is not buffer-local, so we don't
  ;; want to remove this function from that list when we deactivate
  ;; `preview-auto-mode'.
  (if (and preview-auto-mode preview-auto--editing)
      (with-temp-buffer
        (insert str)
        (goto-char (point-min))
        (while (re-search-forward "^[[:space:]]*[\n\r]" nil t)
          (replace-match "" nil nil))
        (buffer-substring-no-properties (point-min) (point-max)))
    str))

(defun preview-auto--find-end-function (region-beg)
  "Find end of LaTeX math environment starting at REGION-BEG.
This takes into account newlines that occur in regions that are
currently being edited.  TODO: write more"
  (or (and preview-auto--editing
           (save-excursion
             (goto-char region-beg)
             (when (looking-at "\\\\begin{[^}]*}")
               (goto-char (match-end 0))
               (LaTeX-find-matching-end))))
      (point)))

;;;###autoload
(define-minor-mode preview-auto-mode
  "Minor mode for running LaTeX preview on a timer."
  :lighter nil
  :group 'preview-auto
  (cond
   (preview-auto-mode
    (progn
      ;; (when (equal (TeX-master-file) "<none>")
      ;;   (user-error "Can't activate preview-auto-mode: either visit a valid TeX file, or set `TeX-master' to one"))
      (unless TeX-header-end
        (setq TeX-header-end LaTeX-header-end))
      (unless TeX-trailer-start
        (setq TeX-trailer-start LaTeX-trailer-start))
      (add-hook 'after-change-functions #'preview-auto--after-change-function nil t)
      (add-hook 'post-command-hook #'preview-auto--post-command-function nil t)
      (when preview-auto--timer
        ;; Reset the timer, in case it's borked.
        (cancel-timer preview-auto--timer)
        (setq preview-auto--timer nil))
      (setq preview-auto--timer (run-with-timer preview-auto-timer-interval
                                                preview-auto-timer-interval
                                                #'preview-auto--timer-function))
      (setq preview-auto--timer-enabled t)
      (setq preview-auto--keepalive t)))
   (t
    (setq preview-auto--timer-enabled nil)
    (remove-hook 'after-change-functions #'preview-auto--after-change-function t)
    (remove-hook 'post-command-hook #'preview-auto--post-command-function t))))

(defun preview-auto-setup ()
  "Hook function for installing bind and menu item."
  (remove-hook 'LaTeX-mode-hook #'preview-auto-setup)  
  (define-key LaTeX-mode-map (kbd "C-c C-p C-a") #'preview-auto-mode)
  (easy-menu-add-item
   nil '("Preview")
   ["automatically" preview-auto-mode]
   "(or toggle) at point"))

(add-hook 'LaTeX-mode-hook #'preview-auto-setup)



(provide 'preview-auto)
;;; preview-auto.el ends here
