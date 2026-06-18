;;;; 设置编辑环境
;; 设置为中文简体语言环境
;(set-language-environment 'Chinese-GB18030)
(set-language-environment 'UTF-8)

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

;;---------------------------------PYIM----------------------------
;;(require 'pyim)
;;(require 'pyim-basedict) ; 拼音词库设置，五笔用户 *不需要* 此行设置
;;(require 'pyim-cregexp-utils)
;;(require 'pyim-cstring-utils)

;; 使用http proxy
;;(setq url-proxy-services
;;      '(("http" . "server.madcat.cc:8888")
;;        ("https" . "server.madcat.cc:8888")
;;        ("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")))

;;(setq pyim-cloudim 'google) ; 云输入法

;; 弹出式菜单
;;(require 'popup)

;; 设置popup的背景
;;(set-face-attribute 'pyim-page nil :background (face-background 'mode-line) :foreground (face-foreground 'mode-line))

;; 加载 basedict 拼音词库。
;;(pyim-basedict-enable)   ; 拼音词库，五笔用户 *不需要* 此行设置

;; 加载 bigdict 拼音词库。
;;(add-to-list 'load-path "~/.emacs.d/plugin.d/pyim-bigdict")
;;(require 'pyim-bigdict)
;;(pyim-bigdict-enable)  ; 拼音词库，五笔用户 *不需要* 此行设置 


;; 将 Emacs 默认输入法设置为 pyim.
;;(setq default-input-method "pyim")

;; 设置 pyim 默认使用的输入法策略，我使用全拼。
;;(pyim-default-scheme 'quanpin)
;; (pyim-default-scheme 'wubi)
;; (pyim-default-scheme 'cangjie)

;; 显示 9 个候选词。
;;(setq pyim-page-length 9)

;; 开启代码搜索中文功能（比如拼音，五笔码等）
;;(pyim-isearch-mode 1)

;; 金手指设置，可以将光标处的编码（比如：拼音字符串）转换为中文。
;;(global-set-key (kbd "M-j") 'pyim-convert-string-at-point)

;; 让云字典在最前 
;;(setq pyim-dcache-icode2word '(pyim-dcache-icode2word pyim-dcache-code2word))

;; 按 "C-<return>" 将光标前的 regexp 转换为可以搜索中文的 regexp.
;;(define-key minibuffer-local-map (kbd "C-RET") 'pyim-cregexp-convert-at-point)

;;-----------------------------RIME--------------------------------------
(require 'rime)
(require 'popup) 

;; 设置必须的路径
(setq rime-emacs-module-header-root "/opt/local/include/")
(setq rime-librime-root "/opt/local/")

;; 将 Emacs-Rime 的配置路径指向你系统鼠须管的路径！
;; 这样能直接复用你前面配置好的简体、雾凇拼音等所有词库设定
(setq rime-user-data-dir "~/Library/Rime")

;; 推荐使用 posframe（悬浮弹窗）来展示候选词条，体验最接近原生输入法
;;(setq rime-show-candidate 'posframe)
;;(setq rime-posframe-style 'horizontal) ; 候选词垂直排列

;; Terminal 下，只能使用popup
(setq rime-show-candidate 'popup)
(setq rime-popup-style 'horizontal) ; 候选词垂直排列

(defun set-popup-face-after-frame-creation()
  ;; 只能裸设置Popup
  (set-face-attribute 'popup-tip-face nil :background (face-background 'mode-line) :foreground (face-foreground 'mode-line)))

(add-hook 'server-after-make-frame-hook 'set-popup-face-after-frame-creation)


;; 其他的魔法设置
(setq rime-disable-predicates
      '(rime-predicate-after-alphabet-char-p    ; 1. 刚敲完一个英文字母，后面继续打字时保持英文
        rime-predicate-prog-in-code-p           ; 2. 【写代码必备】在代码区默认是英文，但只要光标移到代码注释（Comment）或字符串（String）里，自动切回中文
        rime-predicate-space-after-cc-p         ; 3. 在中文字符后面敲一个空格，下一个字自动变成英文（适合写“我今天用了 Emacs”这种中英混排）
        rime-predicate-current-uppercase-letter-p      ; 4. 只要我按住 Shift 敲了一个大写字母，这一段自动变英文
        rime-predicate-org-latex-mode-p))       ; 5. 如果你在用 Org-mode，在 LaTeX 公式块里自动强制英文

;; 设置默认输入法为 rime
(setq default-input-method "rime")

;;----------------------------------------------------------------------

;; 在MACOS的系统中取消了C-SPC，我们把这个按键组合绑定为切换输入
;; 不知道为什么C-SPC和C-@区分不开
(global-set-key (kbd "C-SPC") 'toggle-input-method)
(global-set-key (kbd "C-@") 'toggle-input-method)


(provide 'config-lang)
