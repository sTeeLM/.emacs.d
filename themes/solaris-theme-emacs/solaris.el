;; -*- lexical-binding: t; -*-
;;; solaris.el --- Solaris themes for Emacs

;; Copyright (C) 2020-2021 Thibault Polge

;; Author: Thibaul Polge <thibault@thb.lt>
;; Author: Thomas Frössman <thomasf@jossystem.se>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary

;; Don't use this. Solaris is just a heavily customized version of
;; Solarized. If you like it, fork it and maintain it for yourself,
;; because I don't promise I won't break things every other day --- in
;; fact, I almost promise I *will*.

;;; Code

(defun solaris--heading-height (x) (if solarized-scale-outline-headlines x 1.0))

(defvar solaris-theme
  '("Solaris, like Solarized, but with less fruit salad."
    ;; `default :foreground ,base0 :background ,base03
    (let*
        ((dark (eq variant 'dark))
         ;; Headings for code
         (code-heading-1 `((,class ( :foreground ,red-1fg     :inherit variable-pitch :background ,base02 :weight bold :overline nil :underline ,base03 :extend t))))
         (code-heading-2 `((,class ( :foreground ,magenta-1fg :inherit variable-pitch :background ,base02 :weight bold :overline nil :underline ,base03 :extend t))))
         (code-heading-3 `((,class ( :foreground ,violet-1fg  :inherit variable-pitch :background ,base02 :weight bold :overline nil :underline ,base03 :extend t))))
         (code-heading-4 `((,class ( :foreground ,blue-1fg    :inherit variable-pitch :background ,base02 :weight bold :overline nil :underline ,base03 :extend t))))
         (code-heading-5 `((,class ( :foreground ,cyan-1fg    :inherit variable-pitch :background ,base02 :weight bold :overline nil :underline ,base03 :extend t))))
         (code-heading-6 `((,class ( :foreground ,green-1fg   :inherit variable-pitch :background ,base02 :weight bold :overline nil :underline ,base03 :extend t))))
         (code-heading-7 `((,class ( :foreground ,yellow-1fg  :inherit variable-pitch :background ,base02 :weight bold :overline nil :underline ,base03 :extend t))))
         (code-heading-8 `((,class ( :foreground ,orange-1fg  :inherit variable-pitch :background ,base02 :weight bold :overline nil :underline ,base03)    :extend t)))
         ;; ol-colors
         (doc-heading-1 `((,class (:inherit variable-pitch :foreground ,base02 :distant-foreground ,base2 :height ,(solaris--heading-height 1.6) :background ,base0       :weight normal :slant normal :extend t))))
         (doc-heading-2 `((,class (:inherit variable-pitch :foreground ,base02 :distant-foreground ,base2 :height ,(solaris--heading-height 1.4) :background ,magenta-1bg :weight normal :slant normal :extend t))))
         (doc-heading-3 `((,class (:inherit variable-pitch :foreground ,base02 :distant-foreground ,base2 :height ,(solaris--heading-height 1.2) :background ,violet-1bg  :weight normal :slant normal :extend t))))
         (doc-heading-4 `((,class (:inherit variable-pitch :foreground ,base02 :distant-foreground ,base2 :height ,(solaris--heading-height 1.0) :background ,blue-1bg    :weight normal :slant normal :extend t))))
         (doc-heading-5 `((,class (:inherit variable-pitch :foreground ,base02 :distant-foreground ,base2 :height ,(solaris--heading-height 1.0) :background ,cyan-1bg    :weight bold   :slant normal :extend t))))
         (doc-heading-6 `((,class (:inherit variable-pitch :foreground ,base02 :distant-foreground ,base2 :height ,(solaris--heading-height 1.0) :background ,green-1bg   :weight normal :slant normal :extend t))))
         (doc-heading-7 `((,class (:inherit variable-pitch :foreground ,base02 :distant-foreground ,base2 :height ,(solaris--heading-height 1.0) :background ,yellow-1bg  :weight normal :slant italic :extend t))))
         (doc-heading-8 `((,class (:inherit variable-pitch :foreground ,base02 :distant-foreground ,base2 :height ,(solaris--heading-height 1.0) :background ,orange-1bg  :weight normal :slant italic :extend t)))))
      (custom-theme-set-faces
       theme-name
       ;; display-line-numbers
       `(line-number-minor-tick ((,class (:inherit line-number :weight normal))))
       `(line-number-major-tick ((,class (:inherit line-number-minor-tick :weight bold))))
       `(line-number-current-line ((,class (:inherit line-number :background ,base03 :foreground ,base0))))
       ;; font lock
       `(font-lock-comment-face ((,class (:foreground ,green :slant italic))))
       `(font-lock-comment-delimiter-face ((,class (:foreground ,green :slant italic))))
       `(font-lock-constant-face ((,class (:slant italic))))
       `(font-lock-doc-face ((,class (:foreground ,blue-2fg))))
       `(font-lock-function-name-face ((,class (:foreground ,base2))))
       `(font-lock-keyword-face ((,class (:weight ,s-maybe-bold))))
       `(font-lock-preprocessor-face ((,class (:foreground ,base1))))
       `(font-lock-string-face ((,class (:slant italic))))
       `(font-lock-type-face ((,class (:underline t))))
       `(font-lock-variable-name-face ((,class ())))
       ;; modeline (and header)
       `(mode-line          ((,class (:foreground ,base2 :background ,base02 :underline ,base2 :overline ,base2 :box nil))))
       `(mode-line-inactive ((,class (:foreground ,base01 :background ,base03 :underline ,base02 :overline ,base02 :box nil))))
       `(mode-line-highlight ((,class (:foreground ,base01 :weight ,s-maybe-bold :inverse-video t))))
       `(mode-line-buffer-id ((,class (:foreground ,base3 ))))
       `(header-line ((,class (:inherit mode-line :overline nil))))
       ;; ivy
       `(ivy-current-match ((,class (:background ,blue :foreground ,base03 :weight bold :extend t))))
       ;;;; erc
       `(erc-notice-face ((,class (:foreground ,base01))))
       `(erc-prompt-face ((,class (:background ,base02 :foreground ,base0))))
       `(erc-input-face ((,class (:foreground ,base1))))
       `(erc-timestamp-face ((,class (:foreground ,base02))))
       `(erc-my-nick-face ((,class (:foreground ,base2))))
       `(erc-current-nick-face ((,class (:inverse-video t))))
       `(erc-nick-default-face ((,class (:weight ,s-maybe-bold))))
       ;;;; latext
       `(font-latex-string-face ((,class (:background ,base02))))
       ;;;; links
       `(link ((,class (:foreground ,blue :underline t))))
       ;;;; line numbers
       ;; `(line-number ((,class (:weight normal :underline nil :foreground ,s-fringe-fg :background ,s-fringe-bg))))
       ;;;; markdown
       `(markdown-table-face ((,class (:inherit 'org-table))))
       ;;;; mu4e
       `(mu4e-unread-face ((,class (:weight bold :foreground ,base1))))
       ;;;; org
       `(org-level-1 ,doc-heading-1)
       `(org-level-2 ,doc-heading-2)
       `(org-level-3 ,doc-heading-3)
       `(org-level-4 ,doc-heading-4)
       `(org-level-5 ,doc-heading-5)
       `(org-level-6 ,doc-heading-6)
       `(org-level-7 ,doc-heading-7)
       `(org-level-8 ,doc-heading-8)
       `(org-todo ((,class (:foreground ,base02 :background ,red-1bg :foreground ,red-1fg :box ,red-1fg :underline ,base01))))
       `(org-done ((,class (:foreground ,base02 :background ,green-1bg :foreground ,green-1fg :box ,green-1fg :underline ,base01))))
       ;; @FIXME style org-headline-todo org-headline-done
       `(org-block ((,class (:background ,base03 :extend t))))
       `(org-block-begin-line ((,class (:background ,base02 :foreground ,base01 :underline ,base01 :extend t))))
       `(org-block-end-line ((,class (:inherit org-block-begin-line :underline nil :overline ,base01))))
       `(org-table ((,class (:background ,base02 foreground ,base0))))
       ;;;; outshine/outline-minor
       `(outline-minor-1 ,code-heading-1)
       `(outline-minor-2 ,code-heading-2)
       `(outline-minor-3 ,code-heading-3)
       `(outline-minor-4 ,code-heading-4)
       `(outline-minor-5 ,code-heading-5)
       `(outline-minor-6 ,code-heading-6)
       `(outline-minor-7 ,code-heading-7)
       `(outline-minor-8 ,code-heading-8)
       `(outshine-level-1 ,code-heading-1)
       `(outshine-level-2 ,code-heading-2)
       `(outshine-level-3 ,code-heading-3)
       `(outshine-level-4 ,code-heading-4)
       `(outshine-level-5 ,code-heading-5)
       `(outshine-level-6 ,code-heading-6)
       `(outshine-level-7 ,code-heading-7)
       `(outshine-level-8 ,code-heading-8)
       ;;;; smartparens
       `(sp-show-pair-mismatch-face ((,class (:background ,red :foreground ,yellow :extend t))))
       ;;;; which-function
       `(which-func ((,class (:foreground ,base01))))))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'solaris)
