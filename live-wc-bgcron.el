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


(defun live-wc--count-text-words (&optional complete-buffer)
  "Return a p-list of statistics of words in the buffer.

If a region is selected and COMPLETE-BUFFER is nil, restrict to that region."
  (let* ((num-lines 0)
         (num-bytes 0)
         (num-words 0)
         (restrict (when (and (not complete-buffer) (use-region-p)) t))
         (reg-beg (if restrict (region-beginning) (point-min)))
         (reg-end (if restrict (region-end) (point-max))))
    (save-excursion
      (goto-char reg-beg)
      (while (< (point) reg-end)
        ;; (beginning-of-line)
        (when-let (((cl-notany
                     (lambda (x) (funcall (or (plist-get x :ignore) x)))
                     live-wc-ignore-if))
                   (line-beg (line-beginning-position))
                   (line-end (min (line-end-position) reg-end)))
          (cl-incf num-lines)
          (cl-incf num-bytes (- line-end line-beg))
          (cl-incf num-words (count-words line-beg line-end)))
        (forward-line 1)))
    `((lines . ,num-lines) (bytes . ,num-bytes) (words . ,num-words))))


(defun live-wc--should-count-p ()
  "Should live-wc even count?"
  (and
   live-wc-mode
   (mode-line-window-selected-p)
   (or (buffer-modified-p) (equal live-wc--mem 'uninit))
   (cl-notany (lambda (x) (derived-mode-p x)) live-wc-unbind-modes)))


(defun live-wc--buffer-count ()
  "Stats for buffer counts.

Evaluated as a background process."
  (setq-local live-wc--buffer-stats
              (when (and (live-wc--should-count-p)
                         (not live-wc--buffer-stats)
                         (< (buffer-size) live-wc-max-buffer-size))
                (live-wc--count-text-words t))))


(defun live-wc--region-count ()
  "Stats for selected region

Evaluated as a background process."
  (setq-local live-wc--region-stats
              (when (and (live-wc--should-count-p)
                         (not live-wc--region-stats)
                         (use-region-p)
                         (< (- (region-end) (region-beginning))
                            live-wc-max-buffer-size))
                (live-wc--count-text-words))))


(provide 'live-wc-bgcron)
;;; live-wc-bgcron.el ends here
