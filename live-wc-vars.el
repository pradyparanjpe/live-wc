;;; live-wc-bgcron.el --- variables -*- lexical-binding: t; -*-

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

(defvar-local live-wc-target nil
  "Targetted number of (text) words to write in buffer.

If non-nil, `live-wc-do-count' will use this as the \=TARGET\=.
Value \=0\= is interpreted as nil.
If the value is negative, it is interpreted as \=CAP\= (upper limit).")
(put 'live-wc-target 'safe-local-variable #'numberp)

(defvar-local live-wc-fraction t
  "If non-nil and if possible, show value as a fraction.

If region is selected, display fraction of all the text.
Else, display fraction of `live-wc-target' if set.
Else, fallback to absolute.")
(put 'live-wc-fraction 'safe-local-variable #'booleanp)

(defvar live-wc-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] #'live-wc-set-target)
    (define-key map [mode-line down-mouse-3] #'live-wc-toggle-format)
    map)
  "Keymap to display on word-count indicator.")

(provide 'live-wc-vars)
;;; live-wc-vars.el ends here
