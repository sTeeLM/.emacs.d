;; -*- lexical-binding: t; -*-
(require 'solarized)
(require 'solaris)
(eval-when-compile
  (require 'solarized-palettes))

(deftheme solaris-dark "Solaris dark")
(solarized-with-color-variables
  'dark 'solaris-dark solarized-dark-color-palette-alist solaris-theme)

(provide-theme 'solaris-dark)
