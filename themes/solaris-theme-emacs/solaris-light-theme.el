;; -*- lexical-binding: t; -*-
(require 'solarized)
(require 'solaris)
(eval-when-compile
  (require 'solarized-palettes))

(deftheme solaris-light "Solaris light")
(solarized-with-color-variables
  'light 'solaris-light solarized-light-color-palette-alist solaris-theme)

(provide-theme 'solaris-light)
