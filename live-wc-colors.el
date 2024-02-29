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


(defcustom live-wc-overflow-color "#ff00ff"
  "Color of segment when count overflows `live-wc-target'."
  :type 'color
  :group 'live-wc)


(defcustom live-wc-bright 1.0
  "Brightness of count fraction."
  :type '(number :tag " 0.0 <= bright <= 1.0")
  :group 'live-wc)


(defface live-wc-abs-count
  `((t ,(face-all-attributes 'mode-line-active)))
  "Face for absolute word count.

Color for fraction is determined by function `live-wc--color'."
  :group 'live-wc)


(defun live-wc--invert-color-hex (hex)
  "Return a color hex-string #(0xff-R) (0xff-G) (0xff-B) from HEX."
  (apply #'color-rgb-to-hex `(,@(live-wc--invert-color (color-name-to-rgb hex)) 2)))


(defun live-wc--invert-color (rgb)
  "Return a list of (1-R 1-G 1-B) from RGB."
  (mapcar (lambda (p) (- 1 p)) rgb))


(defun live-wc--fill-color-cap (frac &optional invert overflow)
  "Color based on filled capacity fraction FRAC.

\=0\= is empty (good = cyan), \=1\= is filled (bad = red)
Fraction of brightness is provided through `live-wc-bright'.
FRAC > 1.0 is interpreted as *overfilled* and returns
BRIGHT-scaled `live-wc-overflow-color')
If OVERFLOW is non-nil, return that color for overflow instead.
If INVERT is not-nil, return inverted pallet, i.e.
\=0\= is empty (bad = red), \=1\= is filled (good = cyan)"
  (let* ((frac (if invert (- 1.0 frac) frac))
         (rgb (if (or (> frac 1.0) (< frac 0.0))
                  (color-name-to-rgb (or overflow live-wc-overflow-color))
                (let* ((red (min 1.0 (* 2.0 frac)))
                       (green (min 1.0 (* 2.0 (- 1.0 frac))))
                       (blue (max 0 (- 1.0 (* 10 frac)))))
                  `(,red ,green ,blue))))
         (col-vals (mapcar (lambda (x) (* live-wc-bright x)) rgb)))
    (apply #'color-rgb-to-hex `(,@col-vals 2))))


(defun live-wc--color (frac &optional swap)
  "Translate fraction into color.

FRAC is the fraction (1.0 = full), used to decide color.
Non-nil SWAP swaps \=:background\= and \=:foreground\=."
  (if-let (((mode-line-window-selected-p))
           (disp-color (live-wc--fill-color-cap frac (not swap))))
      (if (and (> frac 1) swap)
          `(:foreground ,(live-wc--invert-color-hex disp-color)
                        :background ,disp-color)
        `(:foreground ,disp-color))
    (face-all-attributes 'mode-line-active)))


(provide 'live-wc-colors)
;;; live-wc-colors.el ends here
