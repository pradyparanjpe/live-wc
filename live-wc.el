;;; live-wc.el --- Count text words in real time -*- lexical-binding: t; -*-

;; Copyright © 2024 Pradyumna Paranjape.

;; Author: Pradyumna Paranjape <pradyparanjpe@rediffmail.com>
;; URL: https://www.gitlab.com/pradyparanjpe/live-wc
;; Version: 0.0.2
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

;;; Code:

(require 'live-wc-vars)
(require 'live-wc-predicates)
(require 'live-wc-custom)
(require 'live-wc-locals)
(require 'live-wc-functions)
(require 'live-wc-commands)
(require 'live-wc-colors)
(require 'live-wc-bgcron)
(require 'live-wc-mmode)
(require 'live-wc-global-mmode)


(provide 'live-wc)
;;; live-wc.el ends here
