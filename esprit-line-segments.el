;;; esprit-line-segments.el --- The segments for esprit-line -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2021  Eder Elorriaga

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

;; The segments for esprit-line

;;; Code:

(require 'subr-x)
(require 'nerd-icons)

(defun esprit-line-make-mouse-map (mouse function)
  "Return a keymap with single entry for mouse key MOUSE on the mode line.
MOUSE is defined to run function FUNCTION with no args in the buffer
corresponding to the mode line clicked."
  (let ((map (make-sparse-keymap)))
    (define-key map (vector 'mode-line mouse) function)
    map))

(defun esprit-line--star-buffer-p (buffer)
  "Return t if BUFFER is a star buffer i.e. *Messages*."
  (string-match-p (rx "*" (0+ nonl) "*") (buffer-name buffer)))

(defun esprit-line--status-indicator-face (read-only modified)
  "Return face for the status indicator given the values of READ-ONLY and MODIFIED."
  (let ((status (cond (read-only "error")
		      (modified "modified")
		      (t "info"))))
    (intern (concat "esprit-line-status-" status))))

(defun esprit-line--status-indicator-icon (read-only modified)
  "Return 'icon' for the status indicator given the values of READ-ONLY and MODIFIED."
  (cond (read-only (nerd-icons-mdicon "nf-md-file_lock_outline"))
	(modified (nerd-icons-mdicon "nf-md-asterisk"))
	(t (nerd-icons-mdicon "nf-md-creation"))))

(defun esprit-line-segment-status-indicator ()
  "Displays a color-coded buffer modification/read-only indicator in the mode-line."
  (if (not (esprit-line--star-buffer-p (current-buffer)))
      (let* ((read-only (and buffer-read-only (buffer-file-name)))
             (modified (buffer-modified-p))
	     (padding esprit-line-padding)
             (status-face (esprit-line--status-indicator-face read-only modified))
	     (top (propertize " " 'face status-face 'display `(raise ,(car padding))))
	     (bot (propertize " " 'face status-face 'display `(raise ,(- (cdr padding))))))
        (concat top
		(propertize
                 (esprit-line--status-indicator-icon read-only modified)
                 'face `(:inherit ,status-face :family ,nerd-icons-font-family))
                bot))))

(defun esprit-line-segment-buffer-name ()
  "Displays the name of the current buffer in the mode-line."
  (propertize "%b " 'face 'esprit-line-status-info))

(defun esprit-line-segment-position ()
  "Displays the current cursor position in the mode-line."
  `((line-number-mode
     ((column-number-mode
       (column-number-indicator-zero-based
        (8 " %l:%c")
        (8 " %l:%C"))
       (5 " L%l")))
     ((column-number-mode
       (column-number-indicator-zero-based
        (5 " C%c")
        (5 " C%C")))))
    ,(if (region-active-p)
         (propertize (format "+%s"
                             (apply #'+ (mapcar
                                         (lambda (pos)
                                           (- (cdr pos)
                                              (car pos)))
                                         (region-bounds))))
                     'font-lock-face 'font-lock-variable-name-face))))

;; 'help-echo (format "End-of-line style: %s\nmouse-1: Cycle" desc)
(defun esprit-line-segment-vc ()
  "Displays color-coded version control information in the mode-line."

  (when vc-mode
    (when-let* ((file (buffer-file-name))
                (branch (substring-no-properties vc-mode 5))
                (state (vc-state file)))
      (propertize (format " %s " (nerd-icons-mdicon "nf-md-source_branch" :face 'mode-line))
                  'help-echo (format "%s, %s" (substring-no-properties vc-mode 5) state)
                  'face 'mode-line))))

(defvar esprit-line-segment-encoding-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1]
                (lambda (e)
	          (interactive "e")
	          (with-selected-window (posn-window (event-start e))
	            (when (and enable-multibyte-characters
		               buffer-file-coding-system)
	              (describe-coding-system buffer-file-coding-system)))))
    (define-key map [mode-line mouse-3]
                (lambda (e)
	          (interactive "e")
	          (with-selected-window (posn-window (event-start e))
	            (call-interactively #'set-buffer-file-coding-system))))
    (purecopy map))
  "Local keymap for the coding-system part of the esprit-line.")

(defun esprit-line-segment-encoding ()
  "Displays the encoding style of the buffer in the mode-line."
  `(" "
    ,(propertize
      "%z"
      'help-echo
      (lambda (window)
        (with-current-buffer (window-buffer window)
          (if buffer-file-coding-system
              (format "Buffer coding system (%s): %s\nmouse-1: Describe coding system\nmouse-3: Set coding system"
                      (if enable-multibyte-characters "multi-byte" "unibyte")
                      (symbol-name buffer-file-coding-system))
            "Buffer coding system: none specified")))
      'mouse-face 'mode-line-highlight
      'local-map esprit-line-segment-encoding-map)))

(defun esprit-line-segment-eol ()
  "Displays the EOL style of the current buffer in the mode-line."
  (let* ((eol (coding-system-eol-type buffer-file-coding-system))
         (mnemonic (pcase eol
                     ('0 (concat " " (nerd-icons-devicon "nf-dev-unix") " "))
                     ('1 (concat " " (nerd-icons-devicon "nf-dev-msdos") " "))
                     ('2 (concat " " (nerd-icons-devicon "nf-dev-apple") " "))
                     (_ "")))
         (desc (pcase eol
                 ('0 "Unix-style")
                 ('1 "DOS-style")
                 ('2 "Mac-style")
                 (_ "Undecided"))))
    (propertize
     mnemonic
     'help-echo (format "End-of-line style: %s\nmouse-1: Cycle" desc)
     'local-map (purecopy
                 (esprit-line-make-mouse-map
                  'mouse-1
                  (lambda (event)
                    (interactive "e")
                    (with-selected-window (posn-window (event-start event))
                      (let ((eol (coding-system-eol-type buffer-file-coding-system)))
                        (set-buffer-file-coding-system
                         (cond ((eq eol 0) 'dos) ((eq eol 1) 'mac) (t 'unix))))))))
     'mouse-face 'mode-line-highlight)))

(defun esprit-line-segment-misc-info ()
  "Displays the current value of `mode-line-misc-info' in the mode-line."
  (let ((misc-info (string-trim (format-mode-line mode-line-misc-info 'esprit-line-unimportant))))
    (unless (string= misc-info "")
      (concat " " misc-info))))

(defun esprit-line-segment-input-method ()
  "Displays the input-method of the buffer in the mode-line."
  `(""
    (current-input-method
     (:propertize (" " current-input-method-title)
                  help-echo (format
                             "Current input method: %s\nmouse-1: Describe current input method"
                             current-input-method)
                  local-map ,(purecopy
                              (esprit-line-make-mouse-map
                               'mouse-1
                               (lambda (e)
                                 (interactive "e")
                                 (with-selected-window (posn-window (event-start e))
                                   (describe-current-input-method)))))
                  mouse-face 'mode-line-highlight))))

(defun esprit-line-segment-minor-modes ()
  "Displays the current minor modes in the mode-line."
  (replace-regexp-in-string
   "%" "%%%%"
   (format-mode-line minor-mode-alist)
   t t))

(defun esprit-line-segment-process ()
  "Displays the current value of `mode-line-process' in the mode-line."
  (when mode-line-process
    (concat " " (string-trim (format-mode-line mode-line-process)))))

(defun esprit-line-segment-major-mode ()
  "Displays the current major mode in the mode-line."
  (concat " "
          (or (and (boundp 'delighted-modes)
                   (cadr (assq major-mode delighted-modes)))
              (format-mode-line (nerd-icons-icon-for-mode major-mode)))
          " "))

(defcustom esprit-line-word-count-modes '(markdown-mode gfm-mode org-mode)
  "Major modes in which to display word count continuously."
  :type '(repeat (symbol :tag "Major-Mode") )
  :group 'esprit-line)

(defun esprit-line-segment-word-count ()
  "Display the buffer word count in the mode-line when in a major mode in `esprit-line-word-count-modes'."
  (if (member major-mode esprit-line-word-count-modes)
      (format " %dW" (count-words (point-min) (point-max)))))


;; (set-fontset-font t 'unicode (font-spec :name "Inconsolata Light" :size 16) nil)
(set-fontset-font t '(#xe000 . #xf8ff) (font-spec :name nerd-icons-font-family) nil)
(set-fontset-font t '(#xf0000 . #xfffff) (font-spec :name nerd-icons-font-family) nil)
(set-fontset-font t '(#x100000 . #x10ffff) (font-spec :name nerd-icons-font-family) nil)

(provide 'esprit-line-segments)
;;; esprit-line-segments.el ends here
