;;; live-wc-custom.el --- customization -*- lexical-binding: t; -*-

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
;;
;;; Commentary:
;;
;; User customization for `live-wc'.
;;
;;; Code:


(require 'live-wc-predicates)


(defgroup live-wc nil
  "Colored display segment of word counts on mode line."
  :group 'convenience
  :group 'display
  :prefix "live-wc")


;; Count when
(defcustom live-wc-unbind-modes '(prog-mode dired-mode special-mode)
  "Major modes and their derivatives, in which, word-count is not displayed.

 This list overrides `live-wc-bind-modes'."
  :type '(repeat (symbol :tag "Mode in which, word count is inactivated"))
  :group 'live-wc)


(defcustom live-wc-idle-sec 1
  "Idle seconds after which, a word count is triggered."
  :type 'number
  :group 'live-wc)


(defcustom live-wc-ignore-if
  '((:ignore live-wc-line-blank-p :desc "blank line")
    (:ignore live-wc-at-comment-p :desc "comment")
    (:ignore org-at-comment-p :desc "org comment")
    (:ignore org-at-keyword-p :desc "org keyword")
    (:ignore org-at-table-p :desc "org table")
    (:ignore org-at-TBLFM-p :desc "org table formula")
    (:ignore org-at-table.el-p :desc "table.el")
    (:ignore org-at-heading-p :desc "org heading")
    (:ignore org-at-property-p :desc "org property")
    (:ignore org-at-drawer-p :desc "org drawer")
    (:ignore org-at-property-drawer-p :desc "property drawer's first line")
    (:ignore live-wc-org-block-range :desc "any org block"))

  "Where any of the functions returns non-nil, ignore the line.

Specifically, if the function returns
  - an integer: jump next to that point (+1).
  - cons cell: jump next to its cdr
  - any other non-nil: forward 1 line."
  :type '(repeat (choice (function :tag "predicate")
                         (plist ((const :ignore) (function :tag "predicate"))
                                ((const :desc) (string :tag "description")))))
  :group 'live-wc)


(defcustom live-wc-max-buffer-size 15360
  "Maximum size of buffer beyond which, word count is inactive."
  :type 'number
  :group 'live-wc)


;; Display
(defcustom live-wc-line-pos most-positive-fixnum
  "Insert live-wc count at this position on mode line.

Remember while setting value that the first two parts of the mode-line
are often \=%e\= and `mode-line-front-space'.

The default value `most-positive-fixnum' puts the segment at the end."
  :type 'number
  :group 'live-wc)


(defcustom live-wc-abs-format "¶:%d"
  "Format of live absolute word count.

\=%d\= (formatted integer) is replaced by the count."
  :type '(string :tag "Must contain a %d")
  :group 'live-wc)


(defcustom live-wc-frac-format "¶:%2.2f%%%%"
  "Format of live word count when expressed as a fraction.

%f (formatted decimal) is replaced by the fraction."
  :type '(string :tag "Must contain a %f")
  :group 'live-wc)


(defcustom live-wc-show-diff t
  "Show difference between count and target."
  :type 'boolean
  :group 'live-wc)


(provide 'live-wc-custom)
;;; live-wc-custom.el ends here
