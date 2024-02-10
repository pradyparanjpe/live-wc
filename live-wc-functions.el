;;; live-wc-functions.el --- external functions -*- lexical-binding: t; -*-

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


(require 'live-wc-internals)
(require 'live-wc-vars)
(require 'live-wc-colors)

(defun live-wc-set-target ()
  "Set value for `live-wc-target'."
  (interactive)
  (let ((wc-target (read-number
                    "Set word count target:\t"
                    (if live-wc-target (- live-wc-target) 0))))
    (setq-local live-wc-target (if (= 0 wc-target) nil wc-target))))

(defun live-wc-toggle-format ()
  "Toggle `live-wc-fraction'."
  (interactive)
  (setq-local live-wc-fraction (not live-wc-fraction)))

(defun live-wc-reset-stats (&optional mem)
  "Reset region and buffer stats, and memory.

Set `live-wc--buffer-stats' `live-wc--region-stats' to nil
and `live-wc--mem' to MEM.
Return MEM.

When called interactively, reset all to nil, return nil."
  (interactive)
  (setq-local live-wc--buffer-stats nil)
  (setq-local live-wc--region-stats nil)
  (setq-local live-wc--org-subtree-stats nil)
  (setq-local live-wc--mem mem)
  mem)


(defun live-wc--add-timers-maybe ()
  "Add timers to `timer-idle-list' to count buffer periodically."
  (unless live-wc--timers
    (setq
     live-wc--timers
     `(,(run-with-idle-timer live-wc-idle-sec t #'live-wc--buffer-count)
       ,(run-with-idle-timer live-wc-idle-sec t #'live-wc--region-count)
       ,(run-with-idle-timer live-wc-idle-sec t #'live-wc--org-count)))))


(defun live-wc--cancel-timers-maybe ()
  "Clean up timers if no buffer (or global) is using it any more."
  (setq live-wc--enabled-buffers
        (cl-delete-if-not (lambda (buf)
                            (or (eq buf 'global)
                                (buffer-live-p buf)))
                          live-wc--enabled-buffers))
  (unless live-wc--enabled-buffers
    (when live-wc--timers
      (dolist (timer live-wc--timers)
        (cancel-timer timer))
      (setq live-wc--timers nil))))

(defun live-wc--goto-org-heading ()
  "Move point to heading of current subtree.

Headings beyond `live-wc--org-headlines-levels' are ignored as =list items=."
  (unless (org-at-heading-p)
    (org-back-to-heading-or-point-min))
  (while (> (or (org-current-level) 0)
            (or live-wc-org-headline-levels
                org-export-headline-levels))
    (unless (= (line-number-at-pos) 1) (previous-line))
    (org-back-to-heading-or-point-min))
  (beginning-of-line))

(defun live-wc--org-bounds ()
  "Bounds of the current org heading.

(ensure that all subtrees are counted for total words)
Compatible with the format of function `region-bounds',
the format is list of cons.
The first (and only) cons is (begin-point . end-point)
of the org heading at point.

Headings at levels less than or equal to
`live-wc-org-headline-levels', which defaults to
`org-export-headline-levels'.
return nil"
  (interactive)
  (if (not (and (featurep 'org) (derived-mode-p 'org-mode)))
      nil
    (if (not (org-current-level))
        ;; Before first heading (not in org-scope)
        nil
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


(defun live-wc--display ()
  ;; This function is called by mode-line again-and-again
  "Generate display string (with properties) for mode-line.

Check if `live-wc--buffer-stats' are available.
Check if `live-wc--org-subtree-stats' are available.
Check if `live-wc--region-stats' are available.
Compose new string based on stats.
Set `live-wc--buffer-stats' and `live-wc--region-stats' to nil as a trigger
for the background counter function `live-wc--buffer-count'
and function `live-wc--region-count'.
Store current stats in memory `live-wc--mem'.

If new stats are unavailable, display from `live-wc--mem'"
  (when (cl-notany (lambda (x) (derived-mode-p x)) live-wc-unbind-modes)
    (if (not (or live-wc--region-stats live-wc--buffer-stats))
        ;; from memory
        live-wc--mem
      ;; calculate
      (let* ((hint (mapconcat (lambda (x)
                                (format "%d %s\n" (cdr x) (car x)))
                              live-wc--buffer-stats))
             (target (when (and live-wc-target (/= live-wc-target 0))
                       (abs live-wc-target)))
             (num-words (or (when live-wc-narrow-to-org-subtree
                              (alist-get 'words live-wc--org-subtree-stats))
                            (alist-get 'words live-wc--buffer-stats)))
             ;; number of words selected
             (num-select (alist-get 'words live-wc--region-stats))
             ;; count-val is either floatp (fraction), integerp
             (count-val (cond
                         ;; Both stats are available and meant to be processed
                         ((and num-words num-select live-wc-fraction)
                          (/ (float num-select) num-words))
                         ;; Only region stats are available
                         (num-select
                          (if target (/ (float num-select) target) num-select))
                         ;; Only buffer stats are available
                         (t (if target (/ (float num-words) target)
                              num-words))))
             (text (if (floatp count-val)
                       (format live-wc-frac-format (* 100 count-val))
                     (format live-wc-abs-format count-val)))
             (disp-face (if (floatp count-val)
                            (live-wc--color
                             count-val
                             (when (and target (> 0 live-wc-target)) t))
                          ;; text is absolute or nil
                          'live-wc-abs-count-face)))
        (live-wc-reset-stats
         (propertize
          text
          'face disp-face
          'local-map live-wc-map
          'help-echo (concat hint (when target (format "of %d" target)))))))))

(provide 'live-wc-functions)
;;; live-wc-functions.el ends here
