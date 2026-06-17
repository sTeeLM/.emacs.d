;; 包安装源设置
(require 'package)

;; 使用官方原版海外源（无镜像）
(setq package-archives '(("gnu"    . "https://gnu.org")
                         ("nongnu" . "https://nongnu.org")
                         ("melpa"  . "https://melpa.org/packages/")))
(package-initialize)

(provide 'config-package)
