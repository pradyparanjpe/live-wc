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

;;; Code:


(require 'live-wc-custom)
(require 'live-wc-internals)


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
  (when-let* ((bounds
               (cond
                ((listp bounds) bounds) ; assume list of cons
                ((integerp bounds) `((,(point-min) . ,bounds)))
                (bounds `((,(point-min) . ,(point-max))))))
              (num-lines 0)
              (num-bytes 0)
              (num-words 0))
    (save-excursion
      (dolist (region bounds)
        (let ((point-begin (car region))
              (point-end (cdr region)))
          (goto-char point-begin)
          (while (< (point) point-end)
            (when-let (((cl-notany
                         (lambda (x) (funcall (or (plist-get x :ignore) x)))
                         live-wc-ignore-if))
                       (line-beg (max (line-beginning-position) point-begin))
                       (line-end (min (line-end-position) point-end)))
              (unless (= line-beg line-end) (cl-incf num-lines))
              (cl-incf num-bytes (- line-end line-beg))
              (cl-incf num-words (count-words line-beg line-end)))
            (forward-line 1)))))
    `((lines . ,num-lines) (bytes . ,num-bytes) (words . ,num-words))))


(defun live-wc--should-count-p ()
  "Should live-wc even count?"
  (and
   live-wc-mode
   (mode-line-window-selected-p)
   (or (buffer-modified-p) (equal live-wc--mem 'uninit))
   (cl-notany (lambda (x) (derived-mode-p x)) live-wc-unbind-modes)))


(defun live-wc--size-ok-p (bounds)
  "Size of scope can be handled.

Returns t only if size (cumulative if region) of scope is less than
`live-wc-max-buffer-size'.

BOUNDS is same parameter/format as accepted by `live-wc--count-text-words'"
  (cond ((integerp bounds) (> live-wc-max-buffer-size (- bounds (point-min))))
        ((listp bounds)
         (> live-wc-max-buffer-size
            (seq-reduce (lambda (y x)
                          (+ y (- (cdr x) (car x))))
                        bounds 0)))
        (bounds (> live-wc-max-buffer-size (- (point-max) (point-min))))))


(defun live-wc--buffer-count ()
  "Stats for buffer counts.

Evaluated as a background process."
  (setq-local live-wc--buffer-stats
              (when (and (live-wc--should-count-p)
                         (not live-wc--buffer-stats)
                         (live-wc--size-ok-p t))
                (live-wc--count-text-words t))))


(defun live-wc--region-count ()
  "Stats for selected region

Evaluated as a background process."
  (setq-local live-wc--region-stats
              (when-let* (((live-wc--should-count-p))
                          ((not live-wc--region-stats))
                          ((use-region-p))
                          (selection (region-bounds))
                          (live-wc--size-ok-p selection))
                (live-wc--count-text-words selection))))


(defun live-wc--org-count ()
  "Stats for Org-mode subtree.

Evaluated as a background process."
  (setq-local live-wc--org-subtree-stats
              (when-let* (((live-wc--should-count-p))
                          ((not live-wc--org-subtree-stats))
                          (org-bounds (live-wc--org-bounds))
                          ((live-wc--size-ok-p org-bounds)))
                (live-wc--count-text-words org-bounds))))


(provide 'live-wc-bgcron)
;;; live-wc-bgcron.el ends here
