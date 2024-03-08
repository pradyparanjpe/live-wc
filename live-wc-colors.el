;;; live-wc-colors.el --- wc-colors -*- lexical-binding: t; -*-

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
;;
;;; Customize and calculate colors

;;; Code:

(require 'live-wc-custom)
(require 'color)


(defface live-wc-overflow
  '((default (:foreground "#004f00" :background "#ff00ff")))
  "Color of segment when count overflows `live-wc-target'."
  :group 'live-wc)


(defface live-wc-abs-count
  `((default 'mode-line-active))
  "Face for absolute word count.

Color for fraction is determined by function `live-wc--color'."
  :group 'live-wc)


(defcustom live-wc-bright 1.0
  "Brightness of count fraction."
  :type '(number :tag " 0.0 <= bright <= 1.0")
  :group 'live-wc)


(defun live-wc--invert-color-hex (hex)
  "Return a color hex-string #(0xff-R) (0xff-G) (0xff-B) from HEX."
  (apply #'color-rgb-to-hex `(,@(live-wc--invert-color (color-name-to-rgb hex)) 2)))


(defun live-wc--invert-color (rgb)
  "Return a list of (1-R 1-G 1-B) from RGB."
  (mapcar (lambda (p) (- 1 p)) rgb))


(defun live-wc--fill-color-cap (frac &optional overflow)
  "Color based on filled capacity fraction absolute value of FRAC.

\\='0\\=' is empty (good = cyan), \\='1\\=' is filled (bad = red)
Fraction of brightness is provided through `live-wc-bright'.
FRAC > 1.0 is interpreted as *overfilled* and returns
OVERFLOW or `live-wc-overflow'."
  (or
   (when-let* ((key (if (> 0 frac) :background :foreground))
               (frac (abs frac))
               ((<= frac 1.0))
               (red (min 1.0 (* 2.0 frac)))
               (green (min 1.0 (* 2.0 (- 1.0 frac))))
               (blue (max 0 (- 1.0 (* 10 frac))))
               (col-vals (mapcar (lambda (x) (* live-wc-bright x))
                                 `(,red ,green ,blue)))
               (color (apply #'color-rgb-to-hex `(,@col-vals 2)))
               (face-attr `(,key ,color)))
     (if (plist-get face-attr :background)
         (if (plist-get face-attr :foreground) face-attr
           (plist-put face-attr :foreground (live-wc--invert-color-hex color)))
       face-attr))
   overflow 'live-wc-overflow))


(defun live-wc--color (frac)
  "Translate fraction into color.

FRAC is the number used to decide color.  If FRAC is integer, return
`live-wc-abs-count'.  If FRAC is more than 1.0, return `live-wc-overflow'.
If FRAC is negative, background is colored.  Else, foreground is colored.
Color is determined based on value of FRAC.  Near 0.0, it is \\='good\\='
more cyan, away from 0.0 in both directions, it is \\='bad\\=' more red."
  (cond ((not (mode-line-window-selected-p)) 'mode-line-inactive)
        ((floatp frac) (live-wc--fill-color-cap frac))
        (t 'live-wc-abs-count)))


(provide 'live-wc-colors)
;;; live-wc-colors.el ends here
