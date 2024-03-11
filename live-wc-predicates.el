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


(defun live-wc--build-cont-end-re (cont-begin-re cont-begin-match cont-end-re)
  "Build live-wc--cont-end-regexp.

Using supplied CONT-END-RE, CONT-BEGIN-RE and CONT-BEGIN-MATCH."
  (save-match-data
    (let* ((re-grpnum-re "\\\\\\(?1:[0-9]+\\)")
           (cont-end-parts (split-string cont-end-re re-grpnum-re))
           (cont-end-refs
            (let ((re-pos 0) refs)
              (while (string-match re-grpnum-re cont-end-re re-pos)
                (push (string-to-number (match-string 1 cont-end-re)) refs)
                (setq re-pos (match-end 1)))
              (reverse refs)))
           (cont-end-refs-alist
            (progn (string-match cont-begin-re cont-begin-match)
                   (mapcar (lambda (x) (match-string x cont-begin-match))
                           cont-end-refs))))
      (apply #'concat
             (mapcan (lambda (n)
                       (list (nth n cont-end-parts)
                             (nth n cont-end-refs-alist)))
                     (number-sequence
                      0 (length cont-end-refs-alist)))))))


(defun live-wc--cont-range (cont-begin-re cont-end-re &optional after before)
  "Return (beginning . end) if inside a container.

Blocks, drawers, properties of `org-mode' are some examples of containers.
Generally, it is region between CONT-BEGIN-RE and CONT-END-RE.

CONT-BEGIN-RE and CONT-END-RE are searched between points AFTER and BEFORE.
If either of AFTER and BEFORE are nil, it is discovered as the previous and
next outline heading.  Such discovery may be overridden by supplying the value
\\=':nil\\=', which enforces the nil value.

Occurrences of \\\\1, \\\\2, ... in CONT-END-RE are replaced with corresponding
match groups of CONT-BEGIN-RE."
  (save-match-data
    (when-let*
        ((case-fold-search t)
         (cont-end-re)
         (after (or after (save-excursion (outline-previous-heading)) :nil))
         (before (or before (save-excursion (outline-next-heading)) :nil))
         (pos (point))
         ((forward-line 0))
         ((re-search-forward cont-begin-re (line-end-position) t))
         ((goto-char (match-end 0)))
         (built-cont-end-re
          (live-wc--build-cont-end-re cont-begin-re
                                      (match-string-no-properties 0)
                                      cont-end-re)))

      (if (not (re-search-forward
                built-cont-end-re (unless (eq :nil before) before) t))
          (not (goto-char pos))
        (goto-char (match-end 0))
        :recheck))))


(defun live-wc-org-block-range (&optional after before)
  "Range of current org block.

AFTER and BEFORE are passed on to `live-wc--cont-range'.

Block is identified by #+BEGIN_<> and #+END_<>
and need not be special block."
  (if (not (featurep 'org)) nil
    (live-wc--cont-range "^[[:blank:]]*#\\+begin_\\(?1:.+?\\)\\(?: .*\\)*$"
                         "^[[:blank:]]*#\\+end_\\1\\( .*\\)*$" after before)))


(defun live-wc-org-drawer-range (&optional after before)
  "Range of current drawer.

AFTER and BEFORE are passed on to `live-wc--cont-range'."
  (if (not (featurep 'org)) nil
    (live-wc--cont-range
     "^[[:blank:]]*:\\(\\(?:\\w\\|[-_]\\)+\\):\\(?: .*\\)*$"
     "^[[:blank:]]*:END:\\(?: .*\\)*$" after before)))


(defun live-wc-line-blank-p ()
  "Point is at a blank line."
  (= (line-beginning-position) (line-end-position)))

(defun live-wc-at-comment-p ()
  "Point is at comment."
  (or (looking-at (format " *%s" comment-start-skip))  ; marker
      (nth 4 (syntax-ppss))))  ; property

(provide 'live-wc-predicates)
;;; live-wc-predicates.el ends here
