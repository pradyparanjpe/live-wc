;;; live-wc-bgcron.el --- background cron -*- lexical-binding: t; -*-

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
;;
;;; Commentary:
;; Functions in this file run in the background when Emacs is idle.
;;
;;; Code:


(require 'live-wc-custom)
(require 'live-wc-vars)
(require 'live-wc-locals)
(require 'live-wc-colors)
(require 'live-wc-functions)


;;; Defined by `org-mode'
(declare-function org-current-level nil)
(declare-function org-narrow-to-subtree nil)


(defvar live-wc-mode)


(defun live-wc--should-count-p ()
  "Should live-wc even count?"
  (and live-wc-mode
       (mode-line-window-selected-p)
       (or live-wc-update-unmodified
           (buffer-modified-p)
           (equal live-wc--mem 'uninit))
       (cl-notany #'derived-mode-p live-wc-unbind-modes)))


(defun live-wc--size-ok-p (bounds)
  "Size of scope can be handled.

Returns t only if size (cumulative if region) of scope is less than
`live-wc-max-buffer-size'.

BOUNDS is same parameter/format as accepted by `live-wc--count-text-words'"
  (cond ((integerp bounds) (> live-wc-max-buffer-size (- bounds (point-min))))
        ((listp bounds)
         (> live-wc-max-buffer-size
            ;; Cumulative of all regions.
            (seq-reduce (lambda (y x) (+ y (- (cdr x) (car x)))) bounds 0)))
        (bounds (> live-wc-max-buffer-size (- (point-max) (point-min))))))


(defun live-wc--count-text-words (&optional bounds)
  "Return a p-list of statistics of words in the buffer.

BOUNDS may be nil, integer, t or compatible with
output of function `region-bounds', a list of cons,
each cons of the form (POINT-BEGIN . POINT-END).
If BOUNDS is nil, return nil.
If BOUNDS is integer, it is interpreted as `((0 . BOUNDS)).
If BOUNDS is t, count for the full buffer.

If POINT-BEGIN and POINT-END are provided, count only that region.
If only POINT-BEGIN is provided, count from that point to the end of buffer.
If neither is provided, count the complete buffer."
  (when-let* ((bounds (cond ((listp bounds) bounds) ; Assume list of cons.
                            ((integerp bounds) `((,(point-min) . ,bounds)))
                            (bounds `((,(point-min) . ,(point-max))))))
              (num-lines 0) (num-chars 0) (num-words 0))
    (save-excursion
      (dolist (region bounds)
        (let ((jump-to (car region)) (point-end (cdr region)))
          (goto-char jump-to)
          (while (< jump-to point-end)
            (let ((non-text
                   (cl-some (lambda (x) (funcall (or (plist-get x :ignore) x)))
                            live-wc-ignore-if))
                  (line-beg (max (line-beginning-position) jump-to))
                  (line-end (min (line-end-position) point-end)))
            (unless non-text
              (cl-incf num-lines)
              (cl-incf num-chars (- line-end line-beg))
              (cl-incf num-words (live-wc--count-words line-beg line-end)))
            (setq jump-to (1+ (pcase non-text
                                ((pred (lambda (x) (eq x :recheck)))
                                 (- (point) 1))
                                ((pred consp) (cdr non-text))
                                ((pred integerp) non-text)
                                ((pred symbolp) (line-end-position)))))
            (goto-char jump-to))))))
    `((lines . ,num-lines) (chars . ,num-chars) (words . ,num-words))))


(defun live-wc--org-bounds ()
  "Bounds of the current org heading.

Ensure that all subtrees are counted for total words.
The format is list of cons following return format of function `region-bounds'.
The only cons in the list is \\='(begin-point . end-point)\\='
of the org heading at point.

Headings at levels beyond `live-wc-org-headline-levels',
which defaults to `org-export-headline-levels', return nil."
  (interactive)
  (if (not (and (featurep 'org) (derived-mode-p 'org-mode))) nil
    (if (not (org-current-level)) nil
      (save-mark-and-excursion
        (let*
            ((org-begin
              (progn
                (when (and (use-region-p) (< (mark) (point)))
                  (exchange-point-and-mark))
                ;; Now, point < mark
                (live-wc--goto-org-heading)
                (point)))
             (org-end
              (progn
                (when (use-region-p) (exchange-point-and-mark))
                ;; Now, mark < point
                (live-wc--goto-org-heading)
                (org-narrow-to-subtree)
                (point-max))))
          (widen)
          `((,org-begin . ,org-end)))))))


;;;###autoload
(defun live-wc--buffer-count ()
  "Stats for buffer counts.

Evaluated as a background process."
  (setq-local live-wc--buffer-stats
              (when (and (live-wc--should-count-p)
                         (not live-wc--buffer-stats)
                         (live-wc--size-ok-p t))
                (live-wc--count-text-words t))))


;;;###autoload
(defun live-wc--region-count ()
  "Stats for selected region.

Evaluated as a background process."
  (setq-local live-wc--region-stats
              (when-let* (((live-wc--should-count-p))
                          ((not live-wc--region-stats))
                          ((use-region-p))
                          (selection (region-bounds))
                          (live-wc--size-ok-p selection))
                (live-wc--count-text-words selection))))


;;;###autoload
(defun live-wc--org-count ()
  "Stats for Org-mode subtree.

Evaluated as a background process."
  (setq-local live-wc--org-subtree-stats
              (when-let* (((live-wc--should-count-p))
                          (live-wc-narrow-to-org-subtree)
                          ((not live-wc--org-subtree-stats))
                          (org-bounds (live-wc--org-bounds))
                          ((live-wc--size-ok-p org-bounds)))
                (live-wc--count-text-words org-bounds))))


(provide 'live-wc-bgcron)
;;; live-wc-bgcron.el ends here
