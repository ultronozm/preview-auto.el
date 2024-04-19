;;; preview-auto.el --- Automatic previews in AUCTeX     -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.0
;; URL: https://github.com/ultronozm/preview-auto.el
;; Package-Requires: ((emacs "26.1") (auctex "14.0.5"))
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

;;; Code:

(require 'cl-lib)
(require 'latex)
(require 'preview)

(defgroup preview-auto nil
  "Settings for preview-auto."
  :group 'preview
  :prefix "preview-auto-")

(defcustom preview-auto-interval 0.3
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

(defvar-local preview-auto--keepalive t
  "Used to keep track of when we should preview some more.")

(defcustom preview-auto-rules-function nil
  "Function to generate rules for identifying math environments.
If non-nil, `preview-auto--generate-rules' delegates to this function.
The function should return a list of rules for identifying math
environments, as described in the documentation of
`preview-auto--generate-rules'."
  :type '(choice (const :tag "Default" nil) function))

(defcustom preview-auto--extra-environments nil
  "Extra environments to consider for automatic previewing."
  :type '(repeat string))

(defvar preview-auto--rules nil
  "Rules for identifying math environments.
Each rule is an iterated cons cell ((BEGIN . END) . PREDICATE), where
BEGIN and END are the delimiters and PREDICATE is a function, called
just beyond the BEGIN delimiter, that returns non-nil if the environment
is valid.")

(defun preview-auto--cheap-texmathp ()
  "Return non-nil if point is in a math environment.
Should work in AUCTeX `LaTeX-mode' buffers.  Implemented using
`font-latex-math-face'."
  (let ((math-face 'font-latex-math-face)
        (face (plist-get (text-properties-at (point))
                         'face)))
    (or (eq face math-face)
        (and (listp face)
             (memq math-face face)))))

(defun preview-auto--generate-rules ()
  "Return list of rules for identifying math environments."
  (if preview-auto-rules-function
      (funcall preview-auto-rules-function)
    (let* ((basic-rules
            (mapcar (lambda (pair)
                      (cons (car pair)
                            (cons (cdr pair) '(preview-auto--cheap-texmathp))))
                    '(("$" . "$") ("$$" . "$$") ("\\(" . "\\)") ("\\[" . "\\]"))))
           (env-rules
            (mapcar (lambda (env)
                      (cons (format "\\begin{%s}" env)
                            (cons (format "\\end{%s}" env) t)))
                    (append texmathp-environments
                            preview-auto--extra-environments)))
           (rules (append basic-rules env-rules)))
      rules)))

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
  (cl-intersection '(active inactive)
                   (mapcar (lambda (ov)
                             (overlay-get ov 'preview-state))
                           (overlays-at (or pos (point))))))

(defcustom preview-auto-predicate nil
  "Additional predicate for determining preview validity.
See the documentation of `preview-auto--next-env' for details."
  :type 'function)

(defun preview-auto--truncated-bound (bound)
  "Return last position before BOUND and any blank lines."
  (save-excursion
    (if (re-search-forward "[\n\r][[:space:]]*[\n\r]" bound t)
        (match-beginning 0)
      bound)))

(defun preview-auto--next-env (bound)
  "Find next LaTeX math environment before BOUND.
Return list of iterated cons cells ((BEGIN . END) . VALIDITY) describing
the bounds for the environment and whether it is considered valid for
preview, which means that (1) it consists of more than just
whitespace, (2) it has not already been previewed, and (3) the
customizable predicate `preview-auto-predicate' holds."
  (catch 'found
    (let ((begin-regexp (regexp-opt (mapcar #'car preview-auto--rules) t)))
      (while (preview-auto--search begin-regexp bound)
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
                 (inner-end (- end (length end-string)))
                 (validity (and (string-match-p "[^[:space:]\n\r]"
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
  '("\\\\begin{abstract}")
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
                    (cl-some
                     (lambda (i)
                       (when
                           (cl-some
                            (lambda (re)
                              (save-excursion
                                (goto-char (cdar (nth i envs)))
                                (preview-auto--search
                                 re (caar (nth (1+ i) envs)))))
                            preview-auto-barriers)
                         (cons (car closest) i)))
                     (number-sequence (car closest) (1- (cdr closest))))
                  (cl-some
                   (lambda (j)
                     (when
                         (cl-some
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

(defvar preview-auto--inhibit-message t
  "If non-nil, inhibit messages in `preview-auto--preview-something'.")

(defun preview-auto--region-wrapper (beg end)
  "Preview region between BEG and END, possibly inhibiting messages."
  (let ((inhibit-message preview-auto--inhibit-message))
    (preview-region beg end)))

(defun preview-auto--update-editing-region ()
  "Update preview of environment being edited."
  (when (texmathp)
    (let ((why (car texmathp-why))
          (begin (cdr texmathp-why)))
      (when (and (not (preview-auto--already-previewed-at begin))
                 (or (null preview-auto-predicate)
                     (funcall preview-auto-predicate)))
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
              ;; Don't preview empty regions.
              (when (string-match-p "[^[:space:]\n\r]"
                                    (buffer-substring-no-properties
                                     (+ begin (length why))
                                     (- end (length end-string))))
                ;; Avoid error-prone updates for multi-line $...$.
                (unless
                    (and (string= why "$")
                         (string-match
                          "[\n\r]" (buffer-substring-no-properties begin end)))
                  (preview-auto--region-wrapper begin end))))))))))

(defun preview-auto--base-range ()
  "Return the base range for previewing.
This is a window around point controlled by the user options
`preview-auto-chars-below' and `preview-auto-chars-above', as well as
the beginning and end of the document."
  (let* ((begin-document
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
    (pcase-let ((`(,pmin . ,pmax) (preview-auto--base-range)))
      (setq preview-auto--keepalive t)
      (cond
       ((when-let ((region (preview-auto--last-valid-region
                            pmin (min pmax (point)))))
          (preview-auto--region-wrapper (car region) (cdr region))))
       ((when-let ((region (preview-auto--first-valid-region
                            (max pmin (point)) pmax)))
          (preview-auto--region-wrapper (car region) (cdr region))))
       ((and
         (< pmin (point) pmax)
         preview-protect-point
         (preview-auto--update-editing-region)))
       (t
        (setq preview-auto--keepalive nil))))))

(defvar preview-auto-mode)

(defun preview-auto--timer-function ()
  "Function called by the preview timer to update LaTeX previews."
  (and (eq major-mode 'LaTeX-mode)
       preview-auto-mode
       preview-auto--timer
       preview-auto--keepalive
       (preview-auto--preview-something)))

(defun preview-auto-conditionally-enable ()
  "Enable `preview-auto-mode' if appropriate.
Check that we are not visiting a bbl file."
  (unless (and (buffer-file-name)
               (string-match-p "\\.bbl\\'" (buffer-file-name)))
    (preview-auto-mode 1)))

(defun preview-auto--after-change (beg _end _length)
  "Hook function for `preview-auto-mode'.
BEG is the start of the modified region, END is the end of the region,
and LENGTH is the length of the modification.  If the modification
occurs before some region where a preview is being generated, then
cancel the preview, so that the preview is not misplaced."
  (when (and preview-current-region
             (< beg (cdr preview-current-region)))
    (ignore-errors (TeX-kill-job))))

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
    (progn
      (unless TeX-header-end
        (setq TeX-header-end LaTeX-header-end))
      (unless TeX-trailer-start
        (setq TeX-trailer-start LaTeX-trailer-start))
      (add-hook 'after-change-functions #'preview-auto--after-change nil t)
      (add-hook 'post-command-hook #'preview-auto--post-command nil t)
      (when preview-auto--timer
        ;; Reset the timer, in case it's borked.
        (cancel-timer preview-auto--timer)
        (setq preview-auto--timer nil))
      (setq preview-auto--timer (run-with-timer preview-auto-interval
                                                preview-auto-interval
                                                #'preview-auto--timer-function))
      (setq preview-auto--keepalive t)))
   (t
    (remove-hook 'after-change-functions #'preview-auto--after-change t)
    (remove-hook 'post-command-hook #'preview-auto--post-command t))))

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
