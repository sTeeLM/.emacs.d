;;配置文件装载路径
(add-to-list 'load-path "~/.emacs.d/init.d/")

(require 'server)

(if (server-running-p)
    (progn
      (message "server already running QUIT!")
      (save-buffers-kill-terminal)))

;;语言设置
(require 'config-lang)

;;窗口界面设置
(require 'config-interface)

;;系统设置
(require 'config-system)

;; 安装源
(require 'config-package)

;;日历设置
(require 'config-calendar)

;;编程设置
(require 'config-programing)

;;邮件设置
(require 'config-mew)

;;vterm设置
(require 'config-vterm)

;;日历设置
(require 'config-dictionary)

;;Markdown设置
(require 'config-markdown)

;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("85024dad36e7fc17103087047990013f36ac6d3081f0ff73692852abf6bba943"
     "885488552855eb50536260f792912f2066227443fea2a487af0437f525018ab4"
     "f4f420a53932aad6d3246d4519dd9dfc94f17d95e41fffaf056fc5c0307ea12e"
     "fc3ac8aa92ad0d472aafbb3d9e0830fcb1f37d9fdc97af4c7fc65df162429dcb"
     "419746ff7bc64bed65f3098540ac51efa2072fbc660da26f0789c0ebf8a34b6a"
     default))
 '(markdown-command "pandoc")
 '(package-selected-packages
   '(color-theme color-theme-modern magit markdown-mode solarized-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
