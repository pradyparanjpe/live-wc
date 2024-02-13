;;; live-wc-custom.el --- minor mode -*- lexical-binding: t; -*-

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
(require 'live-wc-functions)
(require 'live-wc-mmode)


(defvar live-wc--original-default-mode-line nil
  "Stores original mode-line format.")

(put 'live-wc--original-default-mode-line 'risky-local-variable t)


(defun live-wc--modify-default-mode-line ()
  "Modify default `mode-line-format' to include live word count string.

Include `live-wc--line-seg' at position `live-wc-line-pos' if it doesn't
already exist in the mode line perhaps because it was placed there by the
local-version `live-wc--modify-buffer-mode-line'.

Restore using `live-wc-restore-default-mode-line'."
  (when-let* (((consp mode-line-format))
              ((not (member live-wc--line-seg mode-line-format)))
              (insert-at (max 0 (min live-wc-line-pos
                                     (length mode-line-format)))))
    (setq live-wc--original-default-mode-line
          (default-value 'mode-line-format))
    (setq-default
     mode-line-format
     (append (cl-subseq live-wc--original-default-mode-line 0 insert-at)
             `(,live-wc--line-seg)
             (cl-subseq live-wc--original-default-mode-line insert-at)))
    (setq mode-line-format (default-value 'mode-line-format))
    (force-mode-line-update t)))


(defun live-wc--restore-default-mode-line ()
  "Restore the default value of `mode-line-format'.

Reset to the value of `live-wc--original-default-mode-line' if non-nil."
  (when live-wc--original-default-mode-line
    (setq-default mode-line-format live-wc--original-default-mode-line)
    (setq mode-line-format (default-value 'mode-line-format))
    (force-mode-line-update t)))


(defun live-wc--enable-globally ()
  "Functions to be called to enable minor mode."
  (live-wc--add-timers-maybe)
  (add-to-list 'live-wc--enabled-buffers 'global)
  (live-wc--modify-default-mode-line))


(defun live-wc--disable-globally ()
  "Functions to be called to disable minor mode."
  (live-wc--restore-default-mode-line)
  (setq live-wc--enabled-buffers (delete 'global live-wc--enabled-buffers))
  (live-wc--cancel-timers-maybe))


(defun live-wc--turn-on ()
  "Determines if live-wc should be turned on for the buffer."
  (unless (apply #'derived-mode-p live-wc-unbind-modes) (live-wc-mode 1)))


;;;###autoload
(define-globalized-minor-mode global-live-wc-mode
  live-wc-mode
  live-wc--turn-on
  :lighter nil
  :group 'live-wc
  :predicate '(text-mode)
  (if global-live-wc-mode (live-wc--enable-globally)
    (live-wc--disable-globally)))


(provide 'live-wc-global-mmode)
;;; live-wc-global-mmode.el ends here
