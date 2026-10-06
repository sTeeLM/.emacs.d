;; -*- lexical-binding: t; -*-
(require 'solarized)
(require 'solaris)
(eval-when-compile
  (require 'solarized-palettes))

(deftheme solaris-dark-high-contrast "Solaris dark (high contrast)")
(solarized-with-color-variables
  'dark 'solaris-dark-high-contrast solarized-dark-high-contrast-palette-alist solaris-theme)

(provide-theme 'solaris-dark-high-contrast)
