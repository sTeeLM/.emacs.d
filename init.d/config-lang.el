;;;; 设置编辑环境
;; 设置为中文简体语言环境
(set-language-environment 'Chinese-GB18030)

;; 设置emacs 使用 utf-8
(setq locale-coding-system 'utf-8)

;; 设置键盘输入时的字符编码
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; 文件默认保存为 utf-8
(set-buffer-file-coding-system 'utf-8)
(set-default buffer-file-coding-system 'utf8)
(set-default-coding-systems 'utf-8)

;; 解决粘贴中文出现乱码的问题
(set-clipboard-coding-system 'utf-8)

;; 终端中文乱码
(set-terminal-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)
(setq default-process-coding-system '(utf-8 . utf-8))

;; 解决文件目录的中文名乱码
(setq-default pathname-coding-system 'utf-8)

(set-file-name-coding-system 'utf-8)

(prefer-coding-system 'utf-8)

;; 输入法设置
(require 'pyim)
(require 'pyim-basedict) ; 拼音词库设置，五笔用户 *不需要* 此行设置

(require 'popup)
;; 弹出式菜单

;; 加载 basedict 拼音词库。
(pyim-basedict-enable)   ; 拼音词库，五笔用户 *不需要* 此行设置

;; 将 Emacs 默认输入法设置为 pyim.
(setq default-input-method "pyim")

;; 设置 pyim 默认使用的输入法策略，我使用全拼。
(pyim-default-scheme 'quanpin)
;; (pyim-default-scheme 'wubi)
;; (pyim-default-scheme 'cangjie)

;; 显示 9 个候选词。
(setq pyim-page-length 9)

;; 开启代码搜索中文功能（比如拼音，五笔码等）
(pyim-isearch-mode 1)

;; 金手指设置，可以将光标处的编码（比如：拼音字符串）转换为中文。
(global-set-key (kbd "M-j") 'pyim-convert-string-at-point)

;; 按 "C-<return>" 将光标前的 regexp 转换为可以搜索中文的 regexp.
(define-key minibuffer-local-map (kbd "C-<return>") 'pyim-cregexp-convert-at-point)


(provide 'config-lang)
