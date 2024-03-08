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

;;; Commentary:
;;
;; Commands exposed to the user.

;;; Code:


(require 'live-wc-vars)
(require 'live-wc-locals)
(require 'live-wc-functions)


(declare-function org-entry-get nil)  ; Defined by `org-mode'
(declare-function org-set-property nil)  ; Defined by `org-mode'
(declare-function org-current-level nil)  ; Defined by `org-mode'
(declare-function outline-up-heading nil)  ; Required by `org-mode'


;;;###autoload
(defun live-wc-show-target ()
  "Print (message) effective target in print area.

If variable `live-wc-narrow-to-org-subtree' is non-nil and
word count target is set for the heading scope print that.
Else, print local variable `live-wc-target' if that is set."
  (interactive)
  (unless (when-let* ((target (live-wc--get-target))
                      (value (if (= (cdr target) 0) "overridden"
                               (format "%d to %d." (car target)
                                       (cdr target))))
                      (scope
                       (if (eq (car live-wc--scope-target) 'buffer)
                           (buffer-name)
                         ;; We ought to be in org mode
                         (save-mark-and-excursion
                           (live-wc--goto-org-heading)
                           (while-let ((org-level (org-current-level))
                                       ((> org-level 1))
                                       ((not (org-entry-get
                                              (point) "LIVE-WC-TARGET"))))
                             (outline-up-heading 1))
                           (org-entry-get (point) "ITEM" t)))))
            (message "Live word count target for '%s' is %s" scope value))
    (user-error "No word count target in scope")))


;;;###autoload
(defun live-wc-set-org-subtree-target ()
  "Set :PROPERTY: value for \\=':LIVE-WC-TARGET:\\=' for current org sub-tree."
  (interactive)
  (save-mark-and-excursion
    (live-wc--goto-org-heading)
    (unless
        (when-let*
            (((featurep 'org))
             ((derived-mode-p 'org-mode))
             (org-subtree-title (org-entry-get (point) "ITEM" t))
             (org-subtree-target
              (or (live-wc--get-org-subtree-target) :unset))
             (wc-elem-goal
              (read-number
               (format "Set word count goal for '%s':\t" org-subtree-title)
               (if (eq org-subtree-target :unset) 0
                 (live-wc--get-goal org-subtree-target))))
             (wc-elem-cap
              (read-number
               (format "Set word count cap for '%s':\t" org-subtree-title)
               (if (eq org-subtree-target :unset) 0
                 (live-wc--get-cap org-subtree-target)))))
          (org-set-property "LIVE-WC-TARGET"
                            (format "%s" (cons wc-elem-goal wc-elem-cap)))
          org-subtree-target)
    (user-error "Can't set in this context, use `live-wc-set-target'"))))


;;;###autoload
(defun live-wc-set-target ()
  "Set value for `live-wc-target'."
  (interactive)
  (let ((wc-goal (read-number "Set word count goal:\t"
                              (if live-wc-target (live-wc--get-goal) 0)))
        (wc-cap (read-number "Set word count cap:\t"
                             (if live-wc-target (live-wc--get-cap) 0))))
    (setq-local live-wc-target (cons wc-goal wc-cap))))


;;;###autoload
(defun live-wc-toggle-format ()
  "Toggle `live-wc-fraction'."
  (interactive)
  (setq-local live-wc-fraction (not live-wc-fraction)))


;;;###autoload
(defun live-wc-toggle-subtree ()
  "Toggle `live-wc-narrow-to-org-subtree'."
  (interactive)
  (setq-local live-wc-narrow-to-org-subtree
              (not live-wc-narrow-to-org-subtree)))


;;;###autoload
(defun live-wc-toggle-unmodified ()
  "Toggle `live-wc-update-unmodified'."
  (interactive)
  (setq-local live-wc-update-unmodified (not live-wc-update-unmodified)))


;;;###autoload
(defun live-wc-refresh ()
  "Refresh (local) mode-line display segment."
  (interactive)
  (live-wc--reset-stats 'uninit))


(defvar live-wc-keymap
  (let ((map (make-sparse-keymap "live-wc")))
    (define-key map (kbd "f") #'live-wc-toggle-format)
    (define-key map (kbd "o") #'live-wc-set-org-subtree-target)
    (define-key map (kbd "r") #'live-wc-refresh)
    (define-key map (kbd "s") #'live-wc-toggle-subtree)
    (define-key map (kbd "t") #'live-wc-set-target)
    (define-key map (kbd "u") #'live-wc-toggle-unmodified)
    map)
  "Keymap to display on word-count indicator.")


(provide 'live-wc-commands)
;;; live-wc-commands.el ends here
