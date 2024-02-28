;;; live-wc-functions.el --- internal funcs -*- lexical-binding: t; -*-

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

;;; Code:


(require 'live-wc-vars)


;;;###autoload
(defun live-wc--reset-stats (&optional mem)
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


;;;###autoload
(defun live-wc--add-timers-maybe ()
  "Add timers to `timer-idle-list' to count buffer periodically."
  (unless live-wc--timers
    (setq live-wc--timers
          `(,(run-with-idle-timer live-wc-idle-sec t #'live-wc--buffer-count)
            ,(run-with-idle-timer live-wc-idle-sec t #'live-wc--region-count)
            ,(run-with-idle-timer live-wc-idle-sec t #'live-wc--org-count)))))


;;;###autoload
(defun live-wc--cancel-timers-maybe ()
  "Clean up timers if no buffer (or global) is using it any more."
  (setq live-wc--enabled-buffers
        (cl-delete-if-not (lambda (buf)
                            (or (eq buf 'global) (buffer-live-p buf)))
                          live-wc--enabled-buffers))
  (unless live-wc--enabled-buffers
    (when live-wc--timers
      (dolist (timer live-wc--timers) (cancel-timer timer))
      (setq live-wc--timers nil))))


;;;###autoload
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
    (if (not (or live-wc--region-stats
                 live-wc--buffer-stats live-wc--org-subtree-stats
                 (equal live-wc--mem 'uninit)))
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
                         ((and num-words num-select live-wc-fraction
                               (/= num-words 0))
                          (/ (float num-select) num-words))
                         ;; Only region stats are available
                         (num-select
                          (if (and target live-wc-fraction)
                              (/ (float num-select) target)
                            num-select))
                         ;; Only buffer stats are available
                         (t (if (and num-words target live-wc-fraction)
                                (/ (float num-words) target)
                              num-words))))
             (text (cond ((floatp count-val)
                          (format live-wc-frac-format (* 100 count-val)))
                         ((integerp count-val)
                          (format live-wc-abs-format count-val))
                         (t (display-warning
                             '(live-wc format) "Bad count-val type" :debug)
                            "")))
             (disp-face (if (floatp count-val)
                            (live-wc--color
                             count-val
                             (when (and target (> 0 live-wc-target)) t))
                          ;; text is absolute or nil
                          'live-wc-abs-count-face)))
        (live-wc--reset-stats
         (propertize
          text
          'face disp-face
          'local-map live-wc-seg-map
          'help-echo (concat hint (when target (format "of %d" target)))))))))


(provide 'live-wc-functions)
;;; live-wc-functions.el ends here
