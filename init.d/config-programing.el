;;代码折叠
(load-library "hideshow")
(add-hook 'c-mode-hook 'hs-minor-mode)
(add-hook 'c++-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook 'hs-minor-mode)
(add-hook 'perl-mode-hook 'hs-minor-mode)
(add-hook 'php-mode-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
;;能把一个代码块缩起来，需要的时候再展开
;; M-x hs-minor-mode
;; C-c @ ESC C-s show all
;; C-c @ ESC C-h hide all
;; C-c @ C-s show block
;; C-c @ C-h hide block
;; C-c @ C-c toggle hide/show


;;自动补全括号
;;输入左边的括号，就会自动补全右边的部分.包括(), "", [] , {} , 等等。
(defun my-c-mode-auto-pair ()
  (interactive)
  (make-local-variable 'skeleton-pair-alist)
  (setq skeleton-pair-alist '(
    (?` ?` _ "''")
    (?\( ? _ " )")
    (?\[ ? _ " ]")
    (?{ \n > _ \n ?} >)))
  (setq skeleton-pair t)
  (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "{") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "`") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "[") 'skeleton-pair-insert-maybe))
(add-hook 'c-mode-hook 'my-c-mode-auto-pair)
(add-hook 'c++-mode-hook 'my-c-mode-auto-pair)

;;把c语言风格设置为k&r风格
(add-hook 'c-mode-hook
'(lambda ()
(c-set-style "k&r")))

;;css-mode.el编辑css文件
(autoload 'css-mode "css-mode" "CSS editing mode" t)

;;把buffer的内容连同颜色转为html格式
(autoload 'htmlize-buffer "htmlize" "HTMLize mode" t)

;;folding.el 编辑文本的一部分，其它部分折叠起来
(autoload 'folding-mode "folding" "Folding mode" t)
(autoload 'turn-off-folding-mode "folding" "Folding mode" t)
(autoload 'turn-on-folding-mode "folding" "Folding mode" t)

(setq auto-mode-alist
      ;; 将文件模式和文件后缀关联起来
      (append '(("\\.py\\'" . python-mode)
                ("\\.s?html?\\'" . html-helper-mode)
                ("\\.asp\\'" . html-helper-mode)
                ("\\.phtml\\'" . html-helper-mode)
                ("\\.css\\'" . css-mode))
              auto-mode-alist))

;; 添加md模式
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(provide 'config-programing)
