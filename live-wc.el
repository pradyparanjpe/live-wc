;;; live-wc.el --- Count text words in real time -*- lexical-binding: t; -*-

;; Copyright © 2024 Pradyumna Paranjape.

;; Author: Pradyumna Paranjape <pradyparanjpe@rediffmail.com>
;; URL: https://www.gitlab.com/pradyparanjpe/live-wc
;; Version: 0.0.4
;; Package-Requires: ((emacs "29.1"))

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

;;; Commentary:
;;
;; Live word count (`live-wc')
;; ==================================
;;
;; Count text words (excluding code, properties, comments, ...) in real time.
;;
;; Features
;; ------------
;; - `live-wc-target' (local): Set word-count target for the buffer.
;; - `live-wc-fraction' (local): Display word count as
;;   absolute number of words (nil) or as fraction of target (t).
;; - Face color of mode-line display changes according to the fraction.
;; - Restrict word count to region if selected, optionally,
;;   show as fraction of total.
;; - Optionally, restrict word count to org-subtree using
;;   `live-wc-narrow-to-org-subtree'.
;; - Hover (mouse) over the segment for some more details.
;; - Mouse-click to set target/fraction.
;; - Bind keymap `live-wc-keymap' prefix to a suitable keybinding to access
;;   pre-bound commands that toggle/set buffer-local variables.
;;
;; Minor mode
;; ----------------
;; `live-wc-mode': Inserts \\='live-wc\\=' segment in the mode-line.
;; `global-live-wc-mode': Inserts the segment in default mode-line.
;;
;; Customization
;; -------------------
;; Customization group `live-wc' (group `convenience' and group `display') provides
;; customization.
;;
;; `live-wc-mode' is auto-enabled for each mode listed in `global-live-wc-modes'
;; (i.e. `text-mode' by default) and derivatives if the global mode is enabled.

;;; Code:

(require 'live-wc-vars)
(require 'live-wc-predicates)
(require 'live-wc-custom)
(require 'live-wc-locals)
(require 'live-wc-colors)
(require 'live-wc-functions)
(require 'live-wc-commands)
(require 'live-wc-bgcron)
(require 'live-wc-mmode)
(require 'live-wc-global-mmode)


(provide 'live-wc)
;;; live-wc.el ends here
