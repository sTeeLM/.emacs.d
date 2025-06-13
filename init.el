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


;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("57a29645c35ae5ce1660d5987d3da5869b048477a7801ce7ab57bfb25ce12d3e"
     "53b6ea82cb4aa7547d3667b5a007638ff4a2bf877883e440ff3acd82e7bbdabc"
     default))
 '(markdown-command "/opt/local/bin/pandoc")
 '(package-selected-packages '(magit markdown-mode solarized-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
