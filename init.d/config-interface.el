;; 界面配置
;(load-theme 'tango-dark t)
(load-theme 'blue-mood t)

;;(set-scroll-bar-mode nil)
;;取消滚动栏

;;(customize-set-variable 'scroll-bar-mode 'right))
;;设置滚动栏在窗口右侧，而默认是在左侧

;;(tool-bar-mode nil)
;;取消工具栏

;;启动设置
;(setq default-frame-alist
;            '((vertical-scroll-bars)
;              (top . 25)
;               (left . 45)
;               (width . 120)
;               (height . 40)
;               (background-color . "black")
;               (foreground-color . "grey")
;               (cursor-color . "gold1")
;               (mouse-color . "gold1")
;               (tool-bar-lines . 0)
;               (menu-bar-lines . 1)
;               (right-fringe)
;               (left-fringe)))

;;启动自动最大化(数据自己调整，注意格式，如(top . 0)，圆点前后都要留有空格)
(setq initial-frame-alist '((top . 0) (left . 0) (width . 142) (height . 49)))


;; 设置另外一些颜色：语法高亮显示的背景和主题，区域选择的背景和主题，二次选择的背景和选择
(set-face-foreground 'highlight "white")
(set-face-background 'highlight "blue")
(set-face-foreground 'region "cyan")
(set-face-background 'region "blue")
(set-face-foreground 'secondary-selection "skyblue")
(set-face-background 'secondary-selection "darkblue")

(display-time-mode 1);;启用时间显示设置，在minibuffer上面的那个杠上
(setq display-time-24hr-format t);;时间使用24小时制
(setq display-time-day-and-date t);;时间显示包括日期和具体时间
(setq display-time-use-mail-icon t);;时间栏旁边启用邮件设置
(setq display-time-interval 10);;时间的变化频率，单位是秒？

;;显示格式CST 2025/06/22 星期日 20:26:40
(setq display-time-format "%Z %Y/%m/%d %A %H:%M")

(setq font-lock-maximum-decoration t)
(setq font-lock-global-modes '(not shell-mode text-mode))
(setq font-lock-verbose t)
(setq font-lock-maximum-size '((t . 1048576) (vm-mode . 5250000)))
;; 语法高亮。除 shell-mode 和 text-mode 之外的模式中使用语法高亮。

;;显示行列号
(setq mouse-yank-at-point nil)

;;光标靠近鼠标指针时，让鼠标指针自动让开，别挡住视线。
(mouse-avoidance-mode 'animate)

;;在标题栏显示buffer的名字，而不是 emacs@wangyin.com 这样没用的提示。
(setq frame-title-format "emacs@%b")

;;进行语法加亮。
(setq global-font-lock-mode t)

;;修改scratch窗口消息
(setq initial-scratch-message
";; This buffer is for text that is not saved, and for Lisp evaluation. \n\
;; To create a file, visit it with `\\[find-file]' and enter text in its buffer.\n\
;; 使用`\\[mew]'打开邮件，使用`\\[cal]'打开日历\n\
;; 祝你有一个好心情！\n\
")

;; 添加ibuffer作为buffer list的替代
(require 'ibuffer)
(require 'ibuf-ext)

; 原来展示buffer list，现在展示ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)


;; ((predicate "predicate"
;;             #f(compiled-function (buf qualifier) #<bytecode
;;                                  0x3fb8e04ce457962>))
;;  (content "content"
;;           #f(compiled-function (buf qualifier) #<bytecode
;;                                0x159b62c187b845e2>))
;;  (visiting-file "visiting a file"
;;                 #f(compiled-function (buf qualifier) #<bytecode
;;                                      0x9d3f191d399d96a>))
;;  (modified "modified"
;;            #f(compiled-function (buf qualifier) #<bytecode
;;                                 0x5e33519cc4afb1e>))
;;  (size-lt "size less than"
;;           #f(compiled-function (buf qualifier) #<bytecode
;;                                0x1befb789d8f7b78a>))
;;  (size-gt "size greater than"
;;           #f(compiled-function (buf qualifier) #<bytecode
;;                                0x1be97789d8f7a38a>))
;;  (directory "directory name"
;;             #f(compiled-function (buf qualifier) #<bytecode
;;                                  0x53e8da5aa937d06>))
;;  (file-extension "filename extension"
;;                  #f(compiled-function (buf qualifier) #<bytecode
;;                                       0x13615f8054c2cdd5>))
;;  (basename "file basename"
;;            #f(compiled-function (buf qualifier) #<bytecode
;;                                 -0x6c3c16cac8b363c>))
;;  (filename "full file name"
;;            #f(compiled-function (buf qualifier) #<bytecode
;;                                 0x176cb927eda9b22c>))
;;  (starred-name "starred buffer name"
;;                #f(compiled-function (buf qualifier) #<bytecode
;;                                     -0x12b4deaf3b1abae8>))
;;  (process "process"
;;           #f(compiled-function (buf qualifier) #<bytecode
;;                                0x5e33504ffe6fb1e>))
;;  (name "buffer name"
;;        #f(compiled-function (buf qualifier) #<bytecode
;;                             0xdc5a5b9f3bb812e>))
;;  (derived-mode "derived mode"
;;                #f(compiled-function (buf qualifier) #<bytecode
;;                                     0x87cd59349021609>))
;;  (used-mode "major mode in use"
;;             #f(compiled-function (buf qualifier) #<bytecode
;;                                  0x15a5a5da24b2053f>))
;;  (mode "major mode"
;;        #f(compiled-function (buf qualifier) #<bytecode
;;                             0x15a5a5da24b2053f>)))

;分组的定义

(setq ibuffer-diary-markdown ".*/Diary/[0-9][0-9][0-9][0-9]/[0-9][0-9]/[0-9][0-9]/index.md")
(setq ibuffer-diary-preview ".*/Diary/[0-9][0-9][0-9][0-9]/[0-9][0-9]/[0-9][0-9]/index.html")

(setq ibuffer-saved-filter-groups
 '(("MyList"
    ("Dired"    (mode          . dired-mode))    ; Filter by mode
    ("Mail"     (or                                  ; Or multiple!
                (mode . mew-summary-mode)
                (mode . mew-message-mode)))
    ("Term"     (or                                  ; Or multiple!
                (mode . vterm-mode)
                (mode . term-mode)
                (mode . shell-mode)))
    ("Calendar" (or                                  ; Or multiple!
                (mode . calendar-mode)
                (mode . cfw:calendar-mode)
                (mode . cfw:details-mode)))
    ("Diary"    (or
                  (and
                   (filename . ibuffer-diary-markdown)
                   (mode . markdown-mode))
                  (and
                   (filename . ibuffer-diary-preview)
                   (mode . eww-mode))
                  (and
                   (filename . ibuffer-diary-preview)
                   (name . "*lynx*"))
                  (and
                   (filename . ibuffer-diary-preview)
                   (mode . w3m-mode))
                 ))
    ("Stars"    (starred-name))                  ; Group *starred*
    ("Unsaved"  (modified))                      ; All unsaved buffers
    )))

; 默认不展示空分组
(setq ibuffer-show-empty-filter-groups nil)

;; Tell ibuffer to load the group automatically
(defun my-ibuffer-hook ()
  (setq ibuffer-hidden-filter-groups nil) ;初始化的时候，所有分组都展开
  (ibuffer-switch-to-saved-filter-groups "MyList"))
  
(add-hook 'ibuffer-mode-hook 'my-ibuffer-hook)

(provide 'config-interface)
