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
(require 'config-mail)

;;vterm设置
(require 'config-vterm)

;;日历设置
(require 'config-dictionary)

;;Markdown设置
(require 'config-markdown)

;;字体设置
(require 'config-font)
