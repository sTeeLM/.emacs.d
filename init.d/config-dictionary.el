;; 词典配置

(require 'dictionary)

;; 查找本地服务器
(setq dictionary-server "server.home.madcat.cc")

;; C-c l 查找单词
(global-set-key (kbd "C-c l") 'dictionary-lookup-definition)

;; 默认查找中文词典
(setq dictionary-default-dictionary "pydict")

(provide 'config-dictionary) 
