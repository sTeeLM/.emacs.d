;; -*- lexical-binding: t; -*-
(require 'solaris)
(eval-when-compile
  (require 'solaris-palettes))

(deftheme solaris-white "Solaris white")

(solarized-with-color-variables
  'light 'solaris-white solaris-white-color-palette-alist solaris-theme)

(provide-theme 'solaris-white)
