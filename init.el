;;配置文件装载路径
(add-to-list 'load-path "~/.emacs.d/init.d/")

;;语言设置
(require 'config-lang)

;;窗口界面设置
(require 'config-interface)

;;系统设置
(require 'config-system)

;;键盘绑定设置
(require 'config-key)

;;日历设置
(require 'config-calendar)

;;编程设置
(require 'config-programing)

;;邮件设置
;;(require 'config-mew)
