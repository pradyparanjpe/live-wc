;;; live-wc-commands.el --- interactive commands -*- lexical-binding: t; -*-

;; Copyright © 2024 Pradyumna Paranjape.

;; Author: Pradyumna Paranjape <pradyparanjpe@rediffmail.com>
;; URL: https://www.gitlab.com/pradyparanjpe/live-wc

;; This file is NOT part of GNU Emacs.
;; This file is a part of live-wc
;; Some parts of this file were borrowed from PSPMacs
;; PSPMACS URL: https://www.gitlab.com/pradyparanjpe/pspmacs

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
(require 'live-wc-locals)
(require 'live-wc-functions)


;;;###autoload
(defun live-wc-set-target ()
  "Set value for `live-wc-target'."
  (interactive)
  (let ((wc-target
         (read-number "Set word count target:\t"
                      (if live-wc-target (- live-wc-target) 0))))
    (setq-local live-wc-target (if (= 0 wc-target) nil wc-target))))


;;;###autoload
(defun live-wc-toggle-format ()
  "Toggle `live-wc-fraction'."
  (interactive)
  (setq-local live-wc-fraction (not live-wc-fraction)))


;;;###autoload
(defun live-wc-toggle-subtree ()
  "Toggle `live-wc-narrow-to-org-subtree'"
  (interactive)
  (setq-local live-wc-narrow-to-org-subtree
              (not live-wc-narrow-to-org-subtree)))


;;;###autoload
(defun live-wc-toggle-unmodified ()
  "Toggle `live-wc-update-unmodified'"
  (interactive)
  (setq-local live-wc-update-unmodified (not live-wc-update-unmodified)))


;;;###autoload
(defun live-wc-refresh ()
  "Refresh (local) mode-line display segment"
  (interactive)
  (live-wc--reset-stats 'uninit))


(defvar live-wc-seg-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] #'live-wc-set-target)
    (define-key map [mode-line down-mouse-3] #'live-wc-toggle-format)
    map)
  "Keymap to display on word-count indicator.")


(defvar live-wc-keymap
  (let ((map (make-sparse-keymap "live-wc")))
    (define-key map (kbd "f") #'live-wc-toggle-format)
    (define-key map (kbd "r") #'live-wc-refresh)
    (define-key map (kbd "s") #'live-wc-toggle-subtree)
    (define-key map (kbd "t") #'live-wc-set-target)
    (define-key map (kbd "u") #'live-wc-toggle-unmodified)
    map)
  "Keymap to display on word-count indicator.")


(provide 'live-wc-commands)
;;; live-wc-commands.el ends here
