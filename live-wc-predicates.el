;;; live-wc-predicates.el --- test-predicates -*- lexical-binding: t; -*-

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
;; Useful pre-defined predicates

;;; Code:


(require 'outline)

(declare-function org-in-regexp nil)  ; org declares this function


(defun live-wc-org-block-range ()
  "Return (beginning . end) if inside any block.

Does not necessitate Org \\='Special\\=' Block.

This function is heavily adapted from `org-between-regexps-p'.
Gratefully copied from https://scripter.co/splitting-an-org-block-into-two/
Under the function named \\='modi/org-in-any-block-p\\='."
  (if (not (featurep 'org)) nil
    (save-match-data
      (let ((block-begin-re "^[[:blank:]]*#\\+begin_\\(?1:.+?\\)\\(?: .*\\)*$")
            (limit-up (save-excursion (outline-previous-heading)))
            (limit-down (save-excursion (outline-next-heading)))
            (case-fold-search t) (pos (point)) beg end)
        (save-excursion
          ;; Point is on a block when on BLOCK-BEGIN-RE or if
          ;; BLOCK-BEGIN-RE can be found before it...
          (and (or (org-in-regexp block-begin-re)
                   (re-search-backward block-begin-re limit-up :noerror))
               (setq beg (match-beginning 0))
               ;; ... and BLOCK-END-RE after it...
               (let ((block-end-re (concat "^[[:blank:]]*#\\+end_"
                                           (match-string-no-properties 1)
                                           "\\( .*\\)*$")))
                 (goto-char (match-end 0))
                 (re-search-forward block-end-re limit-down :noerror))
               (> (setq end (match-end 0)) pos)
               ;; ... without another BLOCK-BEGIN-RE in-between.
               (goto-char (match-beginning 0))
               (not (re-search-backward block-begin-re (1+ beg) :noerror))
               ;; Return value.
               (cons beg end)))))))


(defun live-wc-line-blank-p ()
  "Point is at a blank line."
  (= (line-beginning-position) (line-end-position)))

(defun live-wc-at-comment-p ()
  "Point is at comment."
  (or (looking-at (format " *%s" comment-start-skip))  ; marker
      (nth 4 (syntax-ppss))))  ; property

(provide 'live-wc-predicates)
;;; live-wc-predicates.el ends here
