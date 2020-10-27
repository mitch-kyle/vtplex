;;; vtermux.el --- terminal multiplexer for vterm. -*- lexical-binding: t -*-

;; Author: Mitch Kyle <mitch.tux@gmail.com>
;; Maintainer: Mitch Kyle <mitch.tux@gmail.com>
;; Version: 0.1.0-alpha
;; Package-Requires: ((vterm "20201004.2057"))
;; Homepage: http://github.com/mitch-kyle/vtermux
;; Keywords: vterm terminal

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:
;; Terminal multiplexer for vterm. similiar to multi-vterm
;; but implemented as a minor mode to be extendible.

;; Features:
;; - Index based navigation
;;     'C-a [0-9]' are used to switch between buffers
;; - Interactive navigation
;;     'C-a b' to switch between buffers
;; - Terminal title tracking
;;     Local variable `vtermux-title' is set to the
;;   terminal title for the buffer

;; Pull requests are welcome to add new functionality

;;; Code:

(require 'vterm)

(declare vtermux-mode)

(defvar vtermux-buffer-list nil
  "list of buffers managed by vtermux.")

;;;;;;;;;;;;;;;;;;;;
;; Terminal Title ;;
;;;;;;;;;;;;;;;;;;;;
;; ???: Maybe make a patch to add this to vterm instead
;;      of hacking it together with advice?
(defvar-local vtermux-title nil
  "Title of the terminal")

(advice-add 'vterm--set-title :before
            (lambda (title)
              (setq vtermux-title title))
            '((name . vtermux-set-title-var)))


;;;;;;;;;;;;;;;;;;;;
;; Hook Functions ;;
;;;;;;;;;;;;;;;;;;;;

(defun vtermux--exit ()
  (setq vtermux-buffer-list
        (delete (current-buffer) vtermux-buffer-list)))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vtermux-switch (&optional title)
  "switch to vtermux buffer by terminal title."
  (interactive)
  (let ((terms (mapcar (lambda (buf)
                         (with-current-buffer buf
                           (cons vtermux-title buf)))
                       (remove (current-buffer)
                               vtermux-buffer-list)))
        (choice (or title
                    (completing-read "vtermux Buffer: "
                                     (map-keys terms)
                                     nil
                                     t))))
    (when-let (buf (assoc title terms))
      (switch-to-buffer buff))))

(defun vtermux-switch-index (index)
  "switch to vtermux buffer by index"
  (if (< -1 index (length vtermux-buffer-list))
      (switch-to-buffer (nth index vtermux-buffer-list))
    (message (format "`vtermux' no buffer at index %d" index))))

(defun vtermux--current-index ()
  "Return the current vtermux index. if not called from a
vtermux buffer the index is 0."
  (or (cl-position (current-buffer)
                   vtermux-buffer-list)
      0))

(defun vtermux-next ()
  "Switch to the next vtermux-buffer."
  (interactive)
  (if vtermux-mode
      (switch-to-buffer
       (nth (mod (1+ (vtermux--current-index))
                 (length vtermux-buffer-list))
            vtermux-buffer-list))
    (vtermux)))

(defun vtermux-prev ()
  "Switch to the previous vtermux buffer."
  (interactive)
  (if vtermux-mode
      (switch-to-buffer
       (nth (mod (1- (vtermux--current-index))
                 (length vtermux-buffer-list))
            vtermux-buffer-list))
    (vtermux)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entry Point Commands ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun vtermux-create ()
 "Start a new vtermux buffer and switch to it."
  (interactive)
  (vterm)
  (vtermux-mode +1))

;;;###autoload
(defun vtermux-execute (command)
  "Execute a shell command in a new vtermux buffer and switch to it."
  (interactive (list (read-shell-command "[vtermux]$ ")))
  (let ((vterm-shell command))
    (vtermux-create)))

;;;###autoload
(defun vtermux ()
  "If a vtermux buffer exists, switch to it, else start a new vtermux buffer"
  (interactive)
  (cond
   (vtermux-mode        nil)
   (vtermux-buffer-list (vtermux-switch-index 0))
   (:else               (vtermux-create))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minor Mode Definition ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar vtermux-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-a C-a") 'vterm-send-C-a)
    (define-key map (kbd "C-a ]")   'vterm-copy-mode)
    (define-key map (kbd "C-a c")   'vtermux-create)
    (define-key map (kbd "C-a r")   'vtermux-execute)
    (define-key map (kbd "C-a b")   'vtermux-switch)
    (define-key map (kbd "C-a n")   'vtermux-next)
    (define-key map (kbd "C-a p")   'vtermux-prev)
    ;; C-a [0-9] - switch to index
    (dotimes (i 10)
      (define-key map (kbd (format "C-a %s" i))
        (lambda ()
          (interactive)
          (vtermux-switch-index i))))
    map))

;;;###autoload
(define-minor-mode vtermux-mode
  "Toggle vtermux managment for vterm buffer"
  :init-value nil
  :lighter "vtermux"
  :keymap vtermux-mode-map
  (if vtermux-mode
      ;; Enable
      (if (not (eq major-mode 'vterm-mode))
          (user-error "`vtermux-mode' can only be used with vterm buffers")
        (progn
          (add-hook 'kill-buffer-hook #'vtermux--exit t)
          (setq vtermux-buffer-list
                (nconc vtermux-buffer-list (list (current-buffer))))))
    ;; Disable
    (progn
      (vtermux--exit)
      (remove-hook 'kill-buffer-hook #'vtermux--exit t))))

(provide 'vtermux)

;;; vtermux.el ends here
