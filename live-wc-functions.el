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
;;
;;; Commentary:
;;
;; Live-wc internal functions.
;;
;;; Code:


;;; Autoloads exceptions
;; According to style guide <https://github.com/bbatsov/emacs-lisp-style-guide>,
;; Internal functions shouldn't be tagged with an \\=';;;###autoload\\=' cookie.
;; However, functions in this file somehow need to be autoloaded for `live-wc'
;; to start working uponing new buffers without throwing errors.
;; Pradyumna Paranjape doesn't claim to completely understand \\='autoloads\\='
;; completely, some or all of these autoloads may be replaced with an alternate,
;; better working code. (Pradyumna Paranjape 2024-02-29)

(require 'cl-lib)
(require 'live-wc-vars)
(require 'live-wc-locals)
(require 'live-wc-custom)
(require 'live-wc-colors)


(declare-function live-wc--buffer-count "live-wc-bgcron")
(declare-function live-wc--region-count "live-wc-bgcron")
(declare-function live-wc--org-count "live-wc-bgcron")
(defvar live-wc-seg-map)


;;; Defined by `org-mode'
(declare-function org-current-level nil)
(declare-function org-entry-get nil)
(declare-function org-at-heading-p nil)
(declare-function org-back-to-heading-or-point-min nil)
(defvar org-link-any-re)


;;;###autoload
(defun live-wc--goto-org-heading ()
  "Move point to heading of current subtree.

Headings beyond `live-wc--org-headlines-levels' are ignored as =list items=."
  (unless (org-at-heading-p) (org-back-to-heading-or-point-min))
  (while (> (or (org-current-level) 0)
            (or live-wc-org-headline-levels
                (if (boundp 'org-export-headline-levels)
                    org-export-headline-levels
                  3)))
    (unless (= (line-number-at-pos) 1) (forward-line -1))
    (org-back-to-heading-or-point-min))
  (beginning-of-line))


;;;###autoload
(defun live-wc--count-words (&optional start end _total)
  "Count words between START and END discounting \\='some\\='.

START and END are normally the start and end of the buffer; but if
the region is active, START and END are the start and end of the region.

Parameters are intended to be compatible with the function `count-words'.
_TOTAL is always ignored (since this function is not *yet* interactive).

Discount (this list will expand):
- All `org-mode' link paths, preserve their description counts."
  (let ((start (or start (if (use-region-p) (region-beginning) (point-min))))
        (end (or end (if (use-region-p) (region-end) (point-max))))
        (words (count-words start end)))
    (when (and (featurep 'org) (derived-mode-p 'org-mode))
      ;; discount link target (keep description) counts
      (save-excursion
        (goto-char start)
        (while-let (((re-search-forward org-link-any-re end t))
                    (beg (or (match-beginning 2) (match-beginning 0)))
                    (end (or (match-end 2) (match-end 0))))
          (setq words (- words (count-words beg end))))))
    words))


;;;###autoload
(defun live-wc--get-target ()
  "Get target for the buffer/org-heading.

If in ORG heading with property \\=':LIVE-WC-TARGET:\\=' is set, return it.
Else, return buffer-local value for `live-wc-target'."
  (or (when-let (((featurep 'org))
                 ((derived-mode-p 'org-mode))
                 (live-wc-narrow-to-org-subtree)
                 (org-subtree-target
                  (when (org-current-level)
                    (org-entry-get (point) "LIVE-WC-TARGET" t))))
        (string-to-number org-subtree-target))
      live-wc-target))


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
  (when (cl-notany #'derived-mode-p live-wc-unbind-modes)
    (if (not (or live-wc--region-stats
                 live-wc--buffer-stats
                 live-wc--org-subtree-stats
                 (equal live-wc--mem 'uninit)))
        live-wc--mem  ; From memory
      ;; calculate
      (let* ((hint (mapconcat (lambda (x)
                                (format "%d %s\n" (cdr x) (car x)))
                              live-wc--buffer-stats))
             (stree-buff-target (live-wc--get-target))
             (target (when (and stree-buff-target (/= stree-buff-target 0))
                       (abs stree-buff-target)))
             (num-words (or (when live-wc-narrow-to-org-subtree
                              (alist-get 'words live-wc--org-subtree-stats))
                            (alist-get 'words live-wc--buffer-stats)))
             ;; Number of words selected
             (num-select (alist-get 'words live-wc--region-stats))
             ;; Count-val is either floatp (fraction), integerp
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
                             (when (and target (> 0 stree-buff-target)) t))
                          ;; text is absolute or nil
                          'live-wc-abs-count)))
        (live-wc--reset-stats
         (propertize
          text
          'face disp-face
          'local-map live-wc-seg-map
          'help-echo (concat hint
                             (when live-wc-target
                               (format "of %d" (abs live-wc-target))))))))))


(provide 'live-wc-functions)
;;; live-wc-functions.el ends here
