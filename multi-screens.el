;;; multi-screens.el --- Minor mode to controlling frames for multiple screens -*- lexical-binding: t; -*-

;;; Time-stamp: <2020-07-02 15:38:37 stardiviner>

;; Authors: stardiviner <numbchild@gmail.com>
;; Package-Requires: ((emacs "25"))
;; Package-Version: 0.1
;; Keywords: frames
;; homepage: https://github.com/stardiviner/multi-screens.el

;; multi-screens is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; multi-screens is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Usage:
;; (add-hook 'after-init-hook #'multi-screens-mode)

;;; Code:

(defgroup multi-screens nil
  "Multi-screens minor mode customization options."
  :prefix "multi-screens-"
  :group 'multi-screens)

(defcustom multi-screens-keybinding-prefix (kbd "C-M-]")
  "Specify multi-screens-mode keybindings prefix before loading.
If it is nil, the default keybindings will not be defined. User can define by yourself."
  :type 'kbd
  :group 'multi-screens)

(defun multi-screens-scroll-other-frame ()
  "Scroll other frame.
This is helpful for multiple monitor screens."
  (interactive)
  (other-frame +1)
  (call-interactively 'scroll-up-command)
  (other-frame -1))

(defun multi-screens-scroll-other-frame-down ()
  "Scroll other frame down.
This is helpful for multiple monitor screens."
  (interactive)
  (other-frame +1)
  (call-interactively 'scroll-down-command)
  (other-frame -1))

(defun multi-screens-window-to-new-frame ()
  "Popup current window into a new frame."
  (interactive)
  (let ((buffer (current-buffer)))
    (unless (one-window-p)
      (delete-window))
    (display-buffer-pop-up-frame buffer nil)))

(defun multi-screens--select-frame ()
  "Select FRAME-NAME interactively."
  (interactive)
  (let ((frame-name (completing-read "multi-screens select frame: "
                                     (mapcar #'car (make-frame-names-alist)))))
    (select-frame (cdr (assoc frame-name (make-frame-names-alist))))))

(defvar multi-screens-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "]") 'multi-screens-scroll-other-frame)
    (define-key map (kbd "[") 'multi-screens-scroll-other-frame-down)
    (define-key map (kbd "5") 'multi-screens-window-to-new-frame)
    map))

(defvar multi-screens-mode-map
  (make-sparse-keymap))

;;;###autoload
(define-minor-mode multi-screens-mode
  "Multi-screens global minor mode to controlling frames for multiple screens."
  :require "multi-screens"
  :global t
  :group 'multi-screens
  :keymap multi-screens-mode-map
  (if multi-screens-mode
      (progn
        (when multi-screens-keybinding-prefix
          (define-key multi-screens-mode-map
            multi-screens-keybinding-prefix multi-screens-prefix-map))
        (message "multi-screens-mode enabled."))
    (message "multi-screens-mode disabled.")))



(provide 'multi-screens)

;;; multi-screens.el ends here
