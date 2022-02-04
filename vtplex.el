;;; vtplex.el --- terminal multiplexer for vterm. -*- lexical-binding: t -*-

;; Author: Mitch Kyle <mitch.tux@gmail.com>
;; Maintainer: Mitch Kyle <mitch.tux@gmail.com>
;; Version: 0.1.0-alpha
;; Package-Requires: ((vterm "20201004.2057"))
;; Homepage: http://github.com/mitch-kyle/vtplex
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
;;     Local variable `vtplex-title' is set to the
;;   terminal title for the buffer

;; Pull requests are welcome to add new functionality

;;; Code:

(require 'vterm)
(require 'seq)

(declare vtplex-mode)

(defvar vtplex-buffer-list nil
  "list of buffers managed by vtplex.")

;;;;;;;;;;;;;;;;;;;;
;; Terminal Title ;;
;;;;;;;;;;;;;;;;;;;;
;; ???: Maybe make a patch to add this to vterm instead
;;      of hacking it together with advice?
(defvar-local vtplex-title nil
  "Title of the terminal")

(advice-add 'vterm--set-title :before
            (lambda (title)
              (setq vtplex-title title))
            '((name . vtplex-set-title-var)))

;;;;;;;;;;;;;;;;;;;;
;; Hook Functions ;;
;;;;;;;;;;;;;;;;;;;;

(defun vtplex--exit ()
  (setq vtplex-buffer-list
        (delete (current-buffer) vtplex-buffer-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vtplex-switch (&optional title)
  "switch to vtplex buffer by terminal title."
  (interactive)
  (let* ((terms (mapcar (lambda (buf)
                          (with-current-buffer buf
                            (cons vtplex-title buf)))
                        (remove (current-buffer)
                                vtplex-buffer-list)))
         (choice (or title
                     (completing-read "vtplex Buffer: "
                                      (map-keys terms)
                                      nil
                                      t))))
    (when-let (buf (cdr (assoc choice terms)))
      (switch-to-buffer buf))))

(defun vtplex-switch-index (index)
  "switch to vtplex buffer by index"
  (if (< -1 index (length vtplex-buffer-list))
      (switch-to-buffer (nth index vtplex-buffer-list))
    (message (format "`vtplex' no buffer at index %d" index))))

(defun vtplex--current-index ()
  "Return the current vtplex index. if not called from a
vtplex buffer the index is 0."
  (or (cl-position (current-buffer)
                   vtplex-buffer-list)
      0))

(defun vtplex-next ()
  "Switch to the next vtplex-buffer."
  (interactive)
  (if vtplex-mode
      (switch-to-buffer
       (nth (mod (1+ (vtplex--current-index))
                 (length vtplex-buffer-list))
            vtplex-buffer-list))
    (vtplex)))

(defun vtplex-prev ()
  "Switch to the previous vtplex buffer."
  (interactive)
  (if vtplex-mode
      (switch-to-buffer
       (nth (mod (1- (vtplex--current-index))
                 (length vtplex-buffer-list))
            vtplex-buffer-list))
    (vtplex)))

(defun vtplex-bury ()
  "Bury vtplex buffers until a non vtplex buffer is displayed"
  (interactive)
  (while vtplex-mode
    (bury-buffer)))

(defun vtplex-hide ()
  "Hide vtplex buffers until a non vtplex buffer is displayed"
  (interactive)
  (let ((switch-to-prev-buffer-skip (lambda (_ buf _)
                                      (with-current-buffer buf
                                        vtplex-mode))))
    (switch-to-prev-buffer)))

(defun vtplex-last ()
  (interactive)
  (when-let (buf (seq-find (lambda (b) (with-current-buffer b vtplex-mode))
                           (buffer-list)))
    (switch-to-buffer buf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entry Point Commands ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun vtplex-create ()
 "Start a new vtplex buffer and switch to it."
  (interactive)
  (vterm)
  (vtplex-mode +1))

;;;###autoload
(defun vtplex-execute (command)
  "Execute a shell command in a new vtplex buffer and switch to it."
  (interactive (list (read-shell-command "[vtplex]$ ")))
  (let ((vterm-shell command))
    (vtplex-create)))

;;;###autoload
(defun vtplex ()
  "If a vtplex buffer exists, switch to it, else start a new vtplex buffer"
  (interactive)
  (cond
   (vtplex-mode        (vtplex-hide))
   (vtplex-buffer-list (vtplex-last))
   (:else              (vtplex-create))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minor Mode Definition ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar vtplex-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-a C-a") 'vterm-send-C-a)
    (define-key map (kbd "C-a [")   'vterm-copy-mode)
    (define-key map (kbd "C-a c")   'vtplex-create)
    (define-key map (kbd "C-a r")   'vtplex-execute)
    (define-key map (kbd "C-a b")   'vtplex-switch)
    (define-key map (kbd "C-a n")   'vtplex-next)
    (define-key map (kbd "C-a p")   'vtplex-prev)
    (define-key map (kbd "C-a d")   'vtplex-bury)
    ;; C-a [0-9] - switch to index
    (dotimes (i 10)
      (define-key map (kbd (format "C-a %s" i))
        (lambda ()
          (interactive)
          (vtplex-switch-index i))))
    map))

;;;###autoload
(define-minor-mode vtplex-mode
  "Toggle vtplex managment for vterm buffer"
  :init-value nil
  :lighter "vtplex"
  :keymap vtplex-mode-map
  (if vtplex-mode
      ;; Enable
      (if (not (eq major-mode 'vterm-mode))
          (user-error "`vtplex-mode' can only be used with vterm buffers")
        (progn
          (add-hook 'kill-buffer-hook #'vtplex--exit t)
          (setq vtplex-buffer-list
                (nconc vtplex-buffer-list (list (current-buffer))))))
    ;; Disable
    (progn
      (vtplex--exit)
      (remove-hook 'kill-buffer-hook #'vtplex--exit t))))

(provide 'vtplex)

;;; vtplex.el ends here
