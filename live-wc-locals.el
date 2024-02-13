;;; live-wc-locals.el --- local-vars -*- lexical-binding: t; -*-

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

Word-count target for the buffer.
Value \=0\= is interpreted as nil.
If the value is negative, it is interpreted as \=CAP\= (upper limit).")

(put 'live-wc-target 'safe-local-variable #'numberp)


(defvar-local live-wc-fraction t
  "If non-nil and if possible, show value as a fraction.

If region is selected, display fraction of all the text.
Else, display fraction of `live-wc-target' if set.
Else, fallback to absolute.")

(put 'live-wc-fraction 'safe-local-variable #'booleanp)


(defvar-local live-wc-org-headline-levels nil
  "Isolate words from org subtree with heading only up to this level.

Beyond this levels, headings are treated as ordinary list items.
If nil, live-wc uses `org-export-headline-levels'")

(put 'live-wc-org-headline-levels 'safe-local-variable
     (lambda (x) (or (not x) (integerp x))))


(defvar-local live-wc-narrow-to-org-subtree t
  "Narrow count to current org subtree whenever possible.")

(put 'live-wc-narrow-to-org-subtree 'safe-local-variable #'booleanp)


(defvar-local live-wc-update-unmodified t
  "Update word counts even when buffer is unmodified.")

(put 'live-wc-update-unmodified 'safe-local-variable #'booleanp)


(provide 'live-wc-locals)
;;; live-wc-locals.el ends here
