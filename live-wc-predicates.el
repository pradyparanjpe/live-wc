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
(defvar markdown-regex-gfm-code-block-open)
(defvar markdown-regex-gfm-code-block-close)


(defun live-wc--build-cont-end-re (cont-begin-re cont-end-re cont-begin-match)
  "Build live-wc--cont-end-regexp.

Using supplied CONT-END-RE, CONT-BEGIN-RE and CONT-BEGIN-MATCH."
  (or  ; Fallback: CONT-END-RE
   (save-match-data
     (when-let*
         ((re-grpnum-re "\\\\\\(?1:[0-9]+\\)")
          ;; Group references in CONT-END-RE (\\1, \\2, ...)
          (cont-end-grps
           (let ((re-pos 0) refs)
             (while (string-match re-grpnum-re cont-end-re re-pos)
               (push (string-to-number (match-string 1 cont-end-re)) refs)
               (setq re-pos (match-end 1)))
             (reverse refs)))
          ;; Associate with what was found while matching CONT-BEGIN-RE
          (grps-alist
           (progn
             (string-match cont-begin-re cont-begin-match)
             (mapcar (lambda (x) (match-string x cont-begin-match))
                     cont-end-grps)))
          ;; CONT-END-RE parts split by references.
          (cont-end-parts (split-string cont-end-re re-grpnum-re)))
       ;; Insert at reference points in CONT-END-RE
       (apply #'concat (mapcan (lambda (n) (list (nth n cont-end-parts)
                                                 (nth n grps-alist)))
                               (number-sequence 0 (length grps-alist))))))
   cont-end-re))


(defun live-wc--skip-cont (cont-begin-re cont-end-re
                                          &optional before case-fold)
  "Return (beginning . end) if inside a container.

Blocks, drawers, properties of `org-mode' are some examples of containers.
Generally, it is region between CONT-BEGIN-RE and CONT-END-RE.

CONT-BEGIN-RE and CONT-END-RE are searched before the point BEFORE.
If BEFORE is nil, it is inferred as the `outline-next-heading'.
Such inferrence may be overridden by supplying the value \\=':nil\\=',
which enforces the nil value.  CASE-FOLD sets `case-fold-search'.

Group references, \\\\1, \\\\2, ... in CONT-END-RE are
replaced with corresponding matches of CONT-BEGIN-RE."
  (save-match-data
    (let ((case-fold-search case-fold)
          (before (unless (eq :nil before)
                    (or before (save-excursion (outline-next-heading)))))
          (pos (point)))
      (when-let*
          ((cont-end-re)
           ((forward-line 0))          ; `beginning-of-line' does not work here.
           ((re-search-forward cont-begin-re (line-end-position) t))
           ((goto-char (match-end 0)))
           (built-cont-end-re
            (live-wc--build-cont-end-re
             cont-begin-re cont-end-re (match-string-no-properties 0))))
        (if (not (re-search-forward built-cont-end-re before t))
            (not (goto-char pos))
          (goto-char (match-end 0))
          :recheck)))))


(defun live-wc-skip-org-block (&optional before)
  "Skip org block at point.

BEFORE is passed on to `live-wc--ship-cont'.

Block is identified by #+BEGIN_(.+?) and corresponding #+END_(\\\\1)
and need not be special block."
  (when (featurep 'org)
    (live-wc--skip-cont "^[[:blank:]]*#\\+begin_\\(?1:.+?\\)\\(?: .*\\)*$"
                        "^[[:blank:]]*#\\+end_\\1\\( .*\\)*$" before t)))


(defun live-wc-skip-org-drawer (&optional before)
  "Skip drawer at point.

BEFORE is passed on to `live-wc--skip-cont'."
  (when (featurep 'org)
    (live-wc--skip-cont
     "^[[:blank:]]*:\\(\\(?:\\w\\|[-_]\\)+\\):[[:blank:]]*$"
     "^[[:blank:]]*:END:\\(?: .*\\)*$" before)))


(defun live-wc-skip-gfm-md-code (&optional before)
  "Skip of code-block in github flavoured markdown.

BEFORE is passed on to `live-wc--skip-cont'."
  (when (and (featurep 'markdown-mode) (derived-mode-p 'markdown-mode))
    (live-wc--skip-cont markdown-regex-gfm-code-block-open
                        markdown-regex-gfm-code-block-close before)))

(defun live-wc-skip-latex-frag (&optional before)
  "Skip LaTeX fragment.

BEFORE is passed on to `live-wc--skip-cont'."
  (when (or (and (featurep 'tex-mode) (derived-mode-p 'tex-mode))
            (and (featurep 'org) (derived-mode-p 'org-mode)))
    (live-wc--skip-cont "^[[:blank:]]*\\$\\$[[:blank:]]*$"
                        "^[[:blank:]]*\\$\\$[[:blank:]]*$" before)))


(defun live-wc-line-blank-p ()
  "Point is at a blank line."
  (= (line-beginning-position) (line-end-position)))

(defun live-wc-at-comment-p ()
  "Point is at comment."
  (or (looking-at (format " *%s" comment-start-skip))  ; marker
      (nth 4 (syntax-ppss))))  ; property

(provide 'live-wc-predicates)
;;; live-wc-predicates.el ends here
