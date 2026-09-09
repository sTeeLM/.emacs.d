;; -*- lexical-binding: t; -*-
;; 包安装源设置
(require 'package)

;; 使用官方原版海外源（无镜像）
;;(setq package-archives '(("gnu"    . "https://gnu.org")
;;                         ("nongnu" . "https://nongnu.org")
;;                         ("melpa"  . "https://melpa.org/packages/")))

(setq package-archives '(("gnu"    . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa"  . "https://melpa.org/packages/")))


;; 配置国内镜像源（推荐）
;;(setq package-archives '(("gnu"   . "https://tsinghua.edu.cn")
;;                         ("melpa" . "https://tsinghua.edu.cn")
;;                         ("nongnu" . "https://tsinghua.edu.cn")))

(package-initialize)

(provide 'config-package)
