

;; 编程相关的所有配置

;; 将文件模式和文件后缀关联起来
(setq auto-mode-alist
      (append '(("\\.py\\'" . python-mode)
                ("\\.css\\'" . css-mode)
                ("\\.c\\'" . c-mode)
                ("\\.h\\'" . c-mode)
                ("\\.cpp\\'" . c++-mode)
                ("\\.el\\'" . emacs-lisp-mode))
              auto-mode-alist))

;;自动补全括号
;;输入左边的括号，就会自动补全右边的部分.包括(), "", [] , {} , 等等。
(defun my-mode-auto-pair ()
  (interactive)
  (make-local-variable 'skeleton-pair-alist)
  (message "my-mode-auto-pair run")  
  (setq skeleton-pair-alist '(
                              '((?\( _ ?\))
				                (?\[ _ ?\])
				                (?{ _ ?})
				                (?< _ ?>)
				                (?' _ ?')
                                (?\" _ ?\"))))
  (setq skeleton-pair t)
  (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "{") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "'") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "[") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "<") 'skeleton-pair-insert-maybe)
  )
(add-hook 'c-mode-hook 'my-mode-auto-pair)
(add-hook 'c++-mode-hook 'my-mode-auto-pair)
(add-hook 'java-mode-hook 'my-mode-auto-pair)
(add-hook 'python-mode-hook 'my-mode-auto-pair)
(add-hook 'emacs-lisp-mode-hook 'my-mode-auto-pair)

;;设置各种语言的编码风格
(defun my-code-style ()
  (message "my-code-style run")
  (progn
    (display-line-numbers-mode)
    (line-number-mode)
    (column-number-mode)
    (whitespace-mode -1)
    (setq c-default-style
	      '((java-mode . "java")
	        (awk-mode . "awk")
	        (other . "linux")))
    (c-set-style "linux")
    (setq c-basic-offset 4)
    (setq tab-width 4)
    (indent-tabs-mode -1)
    (auto-fill-mode -1)
    (xclip-mode 1)
    (setq backward-delete-char-untabify-method nil)))
(add-hook 'c-mode-hook 'my-code-style)
(add-hook 'c++-mode-hook 'my-code-style)
(add-hook 'java-mode-hook 'my-code-style)
(add-hook 'python-mode-hook 'my-code-style)

(defun my-elisp-code-style ()
  (message "my-code-style run")
  (progn
    (display-line-numbers-mode)
    (line-number-mode)
    (column-number-mode)
    (whitespace-mode -1)
    (setq c-basic-offset 4)
    (setq tab-width 4)
    (indent-tabs-mode -1)
    (auto-fill-mode -1)
    (xclip-mode 1)
    (setq backward-delete-char-untabify-method nil)))
(add-hook 'emacs-lisp-mode-hook 'my-elisp-code-style)


;;设置M-g为goto-line
(global-set-key (kbd "M-g") 'goto-line)

;;M+space键设为mark
(global-set-key (kbd "M-SPC") 'set-mark-command)

;;css-mode.el编辑css文件
(autoload 'css-mode "css-mode" "CSS editing mode" t)

;;把buffer的内容连同颜色转为html格式
(autoload 'htmlize-buffer "htmlize" "HTMLize mode" t)

;; 一键格式化
(defun indent-whole ()
  (interactive)
  (indent-region (point-min) (point-max))
  (message "format successfully"))
;; 绑定到F4键
(global-set-key (kbd "<f4>") 'indent-whole)


;; 代码折叠
(load-library "hideshow")
(add-hook 'c-mode-hook 'hs-minor-mode)
(add-hook 'c++-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook 'hs-minor-mode)
(add-hook 'python-mode-hook 'hs-minor-mode)
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

;; run-python C-d可以推出，并且进程推出后自动关闭buffer
(defun kill-inferior-python ()
  (interactive)
  (let ((kill-buffer-query-functions nil))
    (kill-buffer)))

(defun inferior-python-quit-function (process event)
  (message "%s quit" process)
  (let ((buffer (process-buffer process)) )
    (when buffer
      (kill-buffer buffer))))

;; run-python
(defun my-inferior-python-mode-fun ()
  (local-set-key (kbd "C-d") 'kill-inferior-python)
  (let ((process (get-buffer-process (current-buffer))))
    (when process
      (set-process-sentinel process 'inferior-python-quit-function))))
  
(add-hook 'inferior-python-mode-hook 'my-inferior-python-mode-fun)


;; 在窗口底部弹一个小窗，运行交互式程序
;; 反复按可以打开－关闭－打开
(defun toggle-mini-shell (cmd buffer-name)
  (let ((real-buffer-name (format "*%s*" buffer-name)))
  (save-current-buffer
    (unless (get-buffer real-buffer-name)
      (term-run-command cmd buffer-name))
    (if (get-buffer-window real-buffer-name)
        (delete-window (get-buffer-window real-buffer-name))
      (progn
        (display-buffer-in-side-window (get-buffer real-buffer-name) '(side bottom))
        (select-window (get-buffer-window real-buffer-name) 'visible))))))

;; python在toggle-mini-shell由于TERMINFO的问题，不能自动补全，我们特殊处理
(require 'python)
(defun toggle-mini-shell-python (cmd buffer-name)
  (let ((real-buffer-name (format "*%s*" buffer-name)))
  (save-current-buffer
    (unless (get-buffer real-buffer-name)
      (python-shell-make-comint cmd buffer-name))
    (if (get-buffer-window real-buffer-name)
        (delete-window (get-buffer-window real-buffer-name))
      (progn
        (display-buffer-in-side-window (get-buffer real-buffer-name) '(side bottom))
        (select-window (get-buffer-window real-buffer-name) 'visible))))))


;; Python 小窗
;; 可以替代计算器
(defun toggle-python-mini-shell ()
  (interactive)
  (toggle-mini-shell-python "python" "python-mini-shell"))
(global-set-key  (kbd "<f1>") 'toggle-python-mini-shell)


;; CLisp 小窗 
(defun toggle-clisp-mini-shell ()
  (interactive)
  (toggle-mini-shell "clisp" "clisp-mini-shell"))
(global-set-key  (kbd "<f2>") 'toggle-clisp-mini-shell)


;; Bash小窗
;; 可以写程序，然后快速切换到shell，编译，再切换回代码
(defun toggle-bash-mini-shell ()
  (interactive)
  (toggle-mini-shell "bash -i" "bash-mini-shell"))
(global-set-key  (kbd "<f3>") 'toggle-bash-mini-shell)


(provide 'config-programing)
