;;; live-wc-vars.el --- internal vars -*- lexical-binding: t; -*-

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


(defvar-local live-wc--buffer-stats
    nil
  "Buffer stats for the current buffer")


(defvar-local live-wc--region-stats
    nil
  "regions stats for the current selection")


(defvar-local live-wc--org-subtree-stats
    nil
  "Count stats for the current org subtree")


(defvar-local live-wc--mem
    'uninit
  "Memory of displayed value for reuse (esp. while nothing changes)")


(defvar-local live-wc--line-seg
    '(:eval (live-wc--display))
  "Display live word count from `live-word-count-mode'")


(defvar live-wc--timers nil

  "Handle for live-wc timers

For future removal from idle run `timer-idle-list'.
This is not a local variable. It is used by both,
the minor mode and the globalized minor mode.")


(defvar live-wc--enabled-buffers nil

  "A list of buffers for which, live-wc-mode is enabled.")


(dolist (internal
         '(live-wc--buffer-stats
           live-wc--retion-stats
           live-wc--org-subtree-stats
           live-wc--mem
           live-wc--line-seg
           live-wc--timers
           live-wc--enabled-buffers))
  (put internal 'risky-local-variable t))


(provide 'live-wc-vars)
;;; live-wc-vars.el ends here
