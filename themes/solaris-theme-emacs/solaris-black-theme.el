;; -*- lexical-binding: t; -*-
(require 'solaris)
(eval-when-compile
  (require 'solaris-palettes))

(deftheme solaris-black "Solaris black")

(solarized-with-color-variables
  'dark 'solaris-black solaris-white-color-palette-alist solaris-theme)

(provide-theme 'solaris-white)
