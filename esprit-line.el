;;; esprit-line.el --- A sensible mode-line configuration for Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; A sensible mode-line configuration for Emacs.
;; To enable, put this code in your init file:
;; (require 'esprit-line)
;; (esprit-line-mode 1)
;; or
;; (use-package esprit-line
;;   :ensure t
;;   :hook (after-init . esprit-line-mode))
;;

;;; Code:

(require 'esprit-line-segments)

(defgroup esprit-line nil
  "A sensible mode line."
  :prefix "esprit-line-"
  :group 'mode-line)

(defvar esprit-line--default-mode-line mode-line-format
  "The former value of `mode-line-format'.")

;;; Options

(defcustom esprit-line-segments
  '((
     esprit-line-segment-status-indicator
     esprit-line-segment-vc
     esprit-line-segment-buffer-name
     esprit-line-segment-position
     )
    (
     esprit-line-segment-minor-modes
     esprit-line-segment-input-method
     esprit-line-segment-eol
     esprit-line-segment-encoding
     esprit-line-segment-misc-info
     esprit-line-segment-process
     esprit-line-segment-major-mode
     ))
  "Esprit mode-line segments."
  :type '(list (repeat :tag "Left aligned" function)
               (repeat :tag "Right aligned" function))
  :package-version '(esprit-line . "1.2"))

(defcustom esprit-line-padding '(0.25 . 0.25)
  "Default vertical space adjustment (in fraction of character height)."
  :type '(cons (float :tag "Top spacing")
               (float :tag "Bottom spacing"))
  :group 'esprit-line)

;;; Faces

(defun esprit-line--invert-face (face &optional base)
  "Return a spec for FACE with foreground and background swapped.
If provided BASE is used to supply missing attributes."

  (let* ((base (or base 'default))
	 (fg (or (face-foreground face) (face-foreground base)))
	 (bg (or (face-background face) (face-background base))))
    `(:foreground ,bg :background ,fg)))

(defface esprit-line-space
  '((t (:inherit shadow)))
  "Face for space used to alight the right segments in the mode-line.")

(defface esprit-line-unimportant
  '((t (:inherit shadow)))
  "Face for less important mode-line elements.")

(defface esprit-line-status-modified
  `((t (:inherit 'isearch)))
  "Face for the 'modified' indicator symbol in the mode-line.")

(defface esprit-line-status-info
  `((t ,(esprit-line--invert-face 'font-lock-string-face)))
  "Face for generic status indicators in the mode-line.")

(defface esprit-line-status-success
  '((t (:inherit success)))
  "Face used for success status indicators in the mode-line.")

(defface esprit-line-status-warning
  '((t (:inherit warning)))
  "Face for warning status indicators in the mode-line.")

(defface esprit-line-status-error
  '((t (:inherit isearch-fail)))
  "Face for error status indicators in the mode-line.")

;;
;; Helpers
;;

(defun esprit-line--format (left-segments right-segments)
  "Return a string of `window-width' length containing LEFT-SEGMENTS and RIGHT-SEGMENTS, aligned respectively."
  (let* ((left (esprit-line--format-segments left-segments))
         (right (esprit-line--format-segments right-segments))
	 (reserve (length right)))
    (concat
     left
     (propertize " "
                 'display `((space :align-to (- right ,reserve)))
                 'face '(:inherit esprit-line-space))
     right)))

(defun esprit-line--format-segments (segments)
  "Return a string from a list of SEGMENTS."
  (format-mode-line (mapcar
                     (lambda (segment)
                       `(:eval (,segment)))
                     segments)))

(defvar esprit-line--mode-line
  '((:eval
     (esprit-line--format
      (car esprit-line-segments)
      (cadr esprit-line-segments)))))

;;;###autoload
(define-minor-mode esprit-line-mode
  "Minor mode to get a esprit mode line.

When called interactively, toggle
`esprit-line-mode'.  With prefix ARG, enable
`esprit-line--mode' if ARG is positive, otherwise
disable it.

When called from Lisp, enable `esprit-line-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `esprit-line-mode'.
Otherwise behave as if called interactively."
  :init-value nil
  :keymap nil
  :lighter ""
  :group 'esprit-line
  :global t
  (if esprit-line-mode
      ;; Set the new mode-line-format
      (setq-default mode-line-format '(:eval esprit-line--mode-line))
    ;; Restore the original mode-line format
    (setq-default mode-line-format esprit-line--default-mode-line)))

(provide 'esprit-line)
;;; esprit-line.el ends here
