;; -*- lexical-binding: t; -*-
(require 'solarized)
(require 'solaris)
(eval-when-compile
  (require 'solarized-palettes))

(deftheme solaris-light-high-contrast "Solaris light (high contrast)")
(solarized-with-color-variables
  'light 'solaris-light-high-contrast solarized-light-high-contrast-palette-alist solaris-theme)

(provide-theme 'solaris-light-high-contrast)
