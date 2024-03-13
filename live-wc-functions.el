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


;;; Defined by `org-mode'
(declare-function org-current-level nil)
(declare-function org-entry-get nil)
(declare-function org-at-heading-p nil)
(declare-function org-back-to-heading-or-point-min nil)
(defvar org-link-any-re)
(defvar org-keyword-properties)


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


(defun live-wc--string-drop (string drop)
  "Drop from STRING and return residue.

DROP is a list of cons, each corresponding to regions to be dropped
from the original string.  Regions from their car to cdr are dropped."
  (let ((residue string)
        (drop
         (cl-reduce
          (lambda (new-list next-pair)
            "Merge for overlaps."
            (if (null new-list) (push next-pair new-list)
              (if (> (car next-pair) (cdar new-list)) (push next-pair new-list)
                (setf (cdar new-list) (max (cdar new-list) (cdr next-pair)))))
            new-list)
          (sort drop (lambda (x y) (< (car x) (car y))))
          :initial-value nil)))
    (dolist (pair drop)
      (setq residue (concat (substring residue 0 (car pair))
                            (substring residue (cdr pair)))))
    residue))


(defun live-wc--count-occurrences (regex string)
  "Count occurrences of REGEX in STRING."
  (live-wc--recursive-count regex string 0 0))


;; Gratefully derived from
;; https://stackoverflow.com/questions/11847547/emacs-regexp-count-occurrences


(defun live-wc--recursive-count (regex string start counter)
  "Recursively count REGEX in STRING starting at START incrementing COUNTER."
  (if (string-match regex string start)
      (live-wc--recursive-count regex string (match-end 0) (+ counter 1))
    counter))


(defun live-wc--count-words (&optional start end _total)
  "Count words between START and END discounting \\='some\\='.

START and END are normally the start and end of the buffer; but if
the region is active, START and END are the start and end of the region.

Parameters are intended to be compatible with the function `count-words'.
_TOTAL is always ignored (since this function is not *yet* interactive).

Discount (this list will expand):
- All `org-mode' link paths, preserve their description counts.
- All `markdown-mode' link paths, preserve their description counts."
  (let* ((start (or start (if (use-region-p) (region-beginning) (point-min))))
         (end (or end (if (use-region-p) (region-end) (point-max))))
         (line (buffer-substring-no-properties start end)))
    (save-match-data
      (dolist (discount live-wc-discount-inline)
        (let ((predicates (plist-get discount :predicate))
              (modes (mapcar (lambda (x) (intern (format "%s-mode" x)))
                             (plist-get discount :modes)))
              (regex (plist-get discount :regex))
              (groups (plist-get discount :groups))
              (unmodified))
          (when (and (or (not predicates) (cl-every #'eval predicates))
                     (cl-every #'derived-mode-p modes))
            ;; discount link target (keep description) counts
            (while (and (not unmodified)
                        (string-match (if (stringp regex) regex (eval regex))
                                      line))
              (let ((matches
                     (delq nil (mapcar (lambda (x)
                                         (when-let ((beg (match-beginning x))
                                                    (end (match-end x)))
                                           `(,beg . ,end)))
                                       groups))))
                (if (not matches) (setq unmodified t)
                  (setq line (live-wc--string-drop line matches)))))))))
    (live-wc--count-occurrences "\\w+" line)))


(defun live-wc--parse-target (target &optional inner)
  "Parse TARGET info standard from (GOAL . CAP).

TARGET is interpreted as follows:
- cons:      target = TARGET
- positive:  target = (TARGET . `most-positive-fixnum')
- negative:  target = (0 . -TARGET)
- otherwise: target = nil.  (including 0)
User must confirm that 0 <= GOAL <= CAP, are `numberp'.

When optional INNER is provided, it must be a cons of positive numbers.
Any of the cons elements, which is `integerp'
overwrites the corresponding target cons element.
Otherwise, elementwise product with target is returned.
Types of returned numbers (float/integer) are same as types of target."
  (if (not target) inner
    (when-let ((target (cond
                        ((equal target 0) nil)
                        ((consp target) target)
                        ((numberp target)
                         (if (< 0 target) (cons target most-positive-fixnum)
                           (cons 0 (- target))))))
               (inner (or inner '(1.0 . 1.0))))
      (apply #'cons
             (mapcar (lambda (func)
                       (let ((tar-cxr (funcall func target))
                             (scl-cxr (funcall func inner)))
                         (if (integerp scl-cxr) scl-cxr
                           (if (floatp tar-cxr) (* tar-cxr scl-cxr)
                             (ceiling (* tar-cxr scl-cxr))))))
                     '(car cdr))))))


(defun live-wc--get-org-subtree-target ()
  "Get target for the org subtree.

If in ORG heading with property \\=':LIVE-WC-TARGET:\\=' is set,
compose its value based on all parents and return it.  Values may be floats."
  (when (and (featurep 'org)
             (derived-mode-p 'org-mode)
             live-wc-narrow-to-org-subtree
             (org-current-level))
    (let ((buffer-kw-prop
           (car (read-from-string
                 (or (cdr (assoc "LIVE-WC-TARGET" org-keyword-properties))
                     "nil"))))
          (subtree-target))
      (save-excursion
        (live-wc--goto-org-heading)
        (catch 'exit
          (while (org-current-level)
            (setq subtree-target
                  (or
                   (when-let
                       ((parent-target
                         (org-entry-get (point) "LIVE-WC-TARGET")))
                     (live-wc--parse-target
                      (car (read-from-string parent-target))
                      subtree-target))
                   subtree-target))
            (if (<= 1 (org-current-level)) (throw 'exit nil)
              (forward-line -1)
              (org-back-to-heading-or-point-min)))))
      (live-wc--parse-target buffer-kw-prop subtree-target))))


(defun live-wc--get-target (&optional renew)
  "Get target for the buffer/org-heading.

If RENEW is non-nil or `live-wc--scope-target' is nil,
Calculate scope target as follows.
If in ORG heading with property \\=':LIVE-WC-TARGET:\\=' is set,
compose its value based on all parents and buffer-target, then, use it.
Else, use buffer-local value for `live-wc-target'.
Set it to `live-wc--scope-target'.

Return `live-wc--scope-target'."
  (when (or (not live-wc--scope-target) renew)
    (let* ((subtree-target (live-wc--get-org-subtree-target))
           (scope (if subtree-target 'subtree 'buffer))
           (buffer-target (live-wc--parse-target live-wc-target))
           (scope-target (live-wc--parse-target buffer-target subtree-target)))
      (setq-local
       live-wc--scope-target
       (when scope-target
         (cons scope (cons (ceiling (car scope-target))
                           (ceiling (cdr scope-target))))))))
  (cdr live-wc--scope-target))


(defun live-wc--set-target ()
  "Set scope-target for current scope."
  (live-wc--get-target t))


(defun live-wc--get-goal (&optional target)
  "Get goal for the scope.

If TARGET is supplied, else use target from `live-wc--get-target'"
  (car (or (live-wc--parse-target target) (live-wc--get-target))))


(defun live-wc--get-cap (&optional target)
  "Get goal for the scope.

If TARGET is supplied, else use target from `live-wc--get-target'"
  (cdr (or (live-wc--parse-target target) (live-wc--get-target))))


(defun live-wc--make-frac (count &optional target)
  "Convert COUNT into appropriate fraction.

If TARGET is a number, return COUNT / TARGET if (COUNT /= 0), else COUNT.
If TARGET is of the form \\='(GOAL . CAP)
If CAP is zero,  return COUNT.
If GOAL = CAP,   return (COUNT - GOAL) / GOAL.
If COUNT < GOAL, return (COUNT - GOAL) / GOAL (negative fraction).
Else,            return (COUNT - GOAL) / (CAP - GOAL).
If TARGET is nil, use `live-wc--get-target'."
  (if (numberp target) (if (= 0 target) count (/ (float count) target))
    (or (when-let* ((live-wc-fraction)
                    (target (or target (live-wc--get-target)))
                    (cap (live-wc--get-cap target))
                    ((/= 0 cap))
                    (goal (live-wc--get-goal target))
                    (divisor (cond ((or (= cap goal) (> goal count)) goal)
                                   (t (- cap goal)))))
          (/ (float (- count goal)) divisor))
        count)))


(defun live-wc--make-diff (count &optional target)
  "Convert COUNT into signed difference.

If TARGET is a number, return COUNT - TARGET.
If TARGET is nil, use `live-wc--get-target'.
If TARGET is of the form \\='(GOAL . CAP)

Return nil if:
- `live-wc-show-diff' is nil
- Scope target is nil
- count is nil
- CAP = 0 (overridden)
- GOAL < COUNT < CAP

If COUNT < GOAL, DIFF = COUNT - GOAL.
If COUNT > CAP,  DIFF = COUNT - CAP."
  (when-let* ((live-wc-show-diff)
              (count)
              (target (or target (live-wc--get-target)))
              (diff (if (numberp target)
                        (when-let ((abs-diff (- count target))
                                   ((/= 0 abs-diff)))
                          abs-diff)
                      (when-let* ((cap (live-wc--get-cap target))
                                  ((/= 0 cap))
                                  (goal (live-wc--get-goal target))
                                  ((or (< count goal) (< cap count))))
                        (- count (if (< count goal) goal cap)))))
              (diff-str (number-to-string diff)))
    (concat "(" (when (< 0 diff) "+") diff-str ")")))


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


(defun live-wc--add-timers-maybe ()
  "Add timers to `timer-idle-list' to count buffer periodically."
  (unless live-wc--timers
    (setq live-wc--timers
          `(,(run-with-idle-timer live-wc-idle-sec t #'live-wc--buffer-count)
            ,(run-with-idle-timer live-wc-idle-sec t #'live-wc--region-count)
            ,(run-with-idle-timer live-wc-idle-sec t #'live-wc--org-count)
            ,(run-with-idle-timer live-wc-idle-sec t #'live-wc--set-target)))))


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
                          (live-wc--make-frac num-select num-words))
                         ;; Only region stats are available
                         ((and num-select live-wc-fraction)
                          (live-wc--make-frac num-select))
                         ;; Only buffer stats are available
                         (t (live-wc--make-frac num-words))))
             (text (concat
                    (cond ((floatp count-val)
                           (format live-wc-frac-format (* 100 count-val)))
                          ((integerp count-val)
                           (format live-wc-abs-format count-val))
                          (t (display-warning
                              '(live-wc format) "Bad count-val type" :debug)
                             ""))
                    (live-wc--make-diff num-words)))
             (disp-face (live-wc--color count-val)))
        (live-wc--reset-stats
         (propertize
          text
          'face disp-face
          'help-echo (concat hint
                             (when-let (cap (live-wc--get-cap live-wc-target))
                               (format "of %d" cap)))))))))


(provide 'live-wc-functions)
;;; live-wc-functions.el ends here
