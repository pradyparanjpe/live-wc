;;; live-wc-mmode.el --- minor mode -*- lexical-binding: t; -*-

;; Copyright © 2024 Pradyumna Paranjape.

;; Author: Pradyumna Paranjape <pradyparanjpe@rediffmail.com>
;; URL: https://www.gitlab.com/pradyparanjpe/live-wc

;; This file is NOT part of GNU Emacs.
;; This file is a part of live-wc

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:


(require 'live-wc-vars)
(require 'live-wc-custom)
(require 'live-wc-functions)


(defun live-wc--modify-buffer-mode-line ()
  "Modify `mode-line-format' for the buffer to include live word count.

Include `live-wc--line-seg' at position `live-wc-line-pos' if it doesn't
already exist in the mode line perhaps because it was placed there by the
global-version `live-wc--modify-default-mode-line'.

Restore using `live-wc-restore-buffer-mode-line'."
  (when-let* (((consp mode-line-format))
              ((not (member live-wc--line-seg mode-line-format)))
              (insert-at (max 0 (min live-wc-line-pos
                                     (length mode-line-format))))
              (default-form (default-value 'mode-line-format)))
    (setq-local
     mode-line-format
     (append (cl-subseq default-form 0 insert-at)
             `(,live-wc--line-seg)
             (cl-subseq default-form insert-at)))
    (force-mode-line-update t)))


(defun live-wc--restore-buffer-mode-line ()
  "Restore the value of `mode-line-format' for the buffer.

Reset to the value of `live-wc--original-default-mode-line' if non-nil."
  (setq-local mode-line-format (default-value 'mode-line-format))
  (force-mode-line-update t))


(defun live-wc--enable ()
  "Functions to be called to enable minor mode."
  (live-wc--add-timers-maybe)
  (add-to-list 'live-wc--enabled-buffers (current-buffer))
  (add-hook 'kill-buffer-hook #'live-wc--disable 0 t)
  (live-wc--modify-buffer-mode-line))


(defun live-wc--disable ()
  "Functions to be called to disable minor mode."
  (live-wc--restore-buffer-mode-line)
  (setq live-wc--enabled-buffers
        (delq (current-buffer) live-wc--enabled-buffers))
  (remove-hook 'kill-buffer-hook #'live-wc--disable t)
  (live-wc--cancel-timers-maybe))


;;;###autoload
(define-minor-mode live-wc-mode
  "Toggle live-word-count-mode.

When live-word-count-mode is ON, `live-wc--line-seg'
displays current wc value, nil otherwise."
  :lighter " live-wc"
  :keymap nil
  :group 'live-wc
  (if live-wc-mode
      (live-wc--enable)
    (live-wc--disable)))


(provide 'live-wc-mmode)
;;; live-wc-mmode.el ends here
