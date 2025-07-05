;; 中文字体的设置
;; aaaaaaaaaaaaaa
;; 中文字体的自动对齐
;; (require 'cnfonts)
;; 让 cnfonts 在 Emacs 启动时自动生效。
;; (cnfonts-mode 1)
;; 添加两个字号增大缩小的快捷键
;; (define-key cnfonts-mode-map (kbd "C--") #'cnfonts-decrease-fontsize)
;; (define-key cnfonts-mode-map (kbd "C-=") #'cnfonts-increase-fontsize)
;; 显示所有可用的字体
;; (cl-prettyprint (font-family-list))


(defun my-macos-notebook-font()
  "macbook air on 1680x1050."
  (interactive)
  (message "my-macos-notebook-font run")
  (if (eq system-type 'darwin)
      (progn
        ;; https://github.com/ryanoasis/nerd-fonts/tree/master/patched-fonts/UbuntuMono/Regular
        (set-face-attribute 'default nil :font "UbuntuMono Nerd Font Mono 20")

        (setq face-font-rescale-alist '(("苹方-简" . 1)))

        (dolist (charset '(kana han symbol cjk-misc bopomofo))
          (set-fontset-font (frame-parameter nil 'font)
                            charset
                            (font-spec :family "苹方-简" :line-spacing 0)))
        )))

;;(add-hook 'after-make-frame-function 'my-macos-notebook-font)
;;(if (display-graphic-p)
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (when (display-graphic-p frame)
              (with-selected-frame frame
                (my-macos-notebook-font)))))

(provide 'config-font)
