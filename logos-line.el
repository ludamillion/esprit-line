;;; logos-line.el --- A sensible mode-line configuration for Emacs -*- lexical-binding: t; -*-


;;; Commentary:

;; A sensible mode-line configuration for Emacs.
;; To enable, put this code in your init file:
;; (require 'logos-line)
;; (logos-line-mode 1)
;; or
;; (use-package logos-line
;;   :ensure t
;;   :hook (after-init . logos-line-mode))
;;

;;; Code:

(require 'logos-line-segments)

(defgroup logos-line nil
  "A sensible mode line."
  :prefix "logos-line-"
  :group 'mode-line)

(defvar logos-line--default-mode-line mode-line-format
  "The former value of `mode-line-format'.")

;;; Options

(defcustom logos-line-segments
  '((
		 logos-line-segment-status-indicator
     logos-line-segment-vc
     logos-line-segment-buffer-name
     ;; logos-line-segment-position
		 )
    (
		 ;; logos-line-segment-minor-modes
     ;; logos-line-segment-input-method
     ;; logos-line-segment-eol
     ;; logos-line-segment-encoding
     ;; logos-line-segment-misc-info
     ;; logos-line-segment-process
     ;; logos-line-segment-major-mode
		 ))
  "Logos mode-line segments."
  :type '(list (repeat :tag "Left aligned" function)
               (repeat :tag "Right aligned" function))
  :package-version '(logos-line . "1.2"))

(defcustom logos-line-padding '(0.25 . 0.25)
  "Default vertical space adjustment (in fraction of character height)."
  :type '(cons (float :tag "Top spacing")
               (float :tag "Bottom spacing"))
  :group 'logos-line)

;;; Faces

(defun logos-line--invert-face (face &optional base)
  "Return a spec for FACE with foreground and background swapped.
If provided BASE is used to supply missing attributes."

  (let* ((base (or base 'default))
				 (fg (or (face-foreground face) (face-foreground base)))
				 (bg (or (face-background face) (face-background base))))
		`(:foreground ,bg :background ,fg)))

(defface logos-line-space
  '((t (:inherit shadow)))
  "Face for space used to alight the right segments in the mode-line.")

(defface logos-line-unimportant
  '((t (:inherit shadow)))
  "Face for less important mode-line elements.")

(defface logos-line-status-modified
  `((t (:inherit 'isearch)))
  "Face for the 'modified' indicator symbol in the mode-line.")

(defface logos-line-status-info
  `((t ,(logos-line--invert-face 'font-lock-string-face)))
  "Face for generic status indicators in the mode-line.")

(defface logos-line-status-success
  '((t (:inherit success)))
  "Face used for success status indicators in the mode-line.")

(defface logos-line-status-warning
  '((t (:inherit warning)))
  "Face for warning status indicators in the mode-line.")

(defface logos-line-status-error
  '((t (:inherit isearch-fail)))
  "Face for error status indicators in the mode-line.")

;;
;; Helpers
;;

(defun logos-line--format (left-segments right-segments)
  "Return a string of `window-width' length containing LEFT-SEGMENTS and RIGHT-SEGMENTS, aligned respectively."
  (let* ((left (logos-line--format-segments left-segments))
         (right (logos-line--format-segments right-segments))
				 (reserve (length right)))
    (concat
     left
     (propertize " "
                 'display `((space :align-to (- right ,reserve)))
                 'face '(:inherit logos-line-space))
     right)))

(defun logos-line--format-segments (segments)
  "Return a string from a list of SEGMENTS."
  (format-mode-line (mapcar
                     (lambda (segment)
                       `(:eval (,segment)))
                     segments)))

(defvar logos-line--mode-line
  '((:eval
     (logos-line--format
      (car logos-line-segments)
      (cadr logos-line-segments)))))

;;;###autoload
(define-minor-mode logos-line-mode
  "Minor mode to get a logos mode line.

When called interactively, toggle
`logos-line-mode'.  With prefix ARG, enable
`logos-line--mode' if ARG is positive, otherwise
disable it.

When called from Lisp, enable `logos-line-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `logos-line-mode'.
Otherwise behave as if called interactively."
  :init-value nil
  :keymap nil
  :lighter ""
  :group 'logos-line
  :global t
  (if logos-line-mode
      ;; Set the new mode-line-format
      (setq-default mode-line-format '(:eval logos-line--mode-line))
    ;; Restore the original mode-line format
    (setq-default mode-line-format logos-line--default-mode-line)))

(provide 'logos-line)
;;; logos-line.el ends here
