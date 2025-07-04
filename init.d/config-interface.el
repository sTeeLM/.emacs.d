;; 界面配置
(load-theme 'classic t)
;;(load-theme 'blue-mood t)

;;(set-scroll-bar-mode nil)
;;取消滚动栏

;;(customize-set-variable 'scroll-bar-mode 'right))
;;设置滚动栏在窗口右侧，而默认是在左侧

;;(tool-bar-mode nil)
;;取消工具栏

;;启动设置
;;(setq default-frame-alist
;;            '((vertical-scroll-bars)
;;              (top . 25)
;;               (left . 45)
;;               (width . 120)
;;               (height . 40)
;;               (background-color . "black")
;;               (foreground-color . "grey")
;;               (cursor-color . "gold1")
;;               (mouse-color . "gold1")
;;               (tool-bar-lines . 0)
;;               (menu-bar-lines . 1)
;;               (right-fringe)
;;               (left-fringe)))

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


;; ;;添加ibuffer作为buffer list的替代
(require 'ibuffer)
(require 'ibuf-ext)
;;分组的定义
(setq ibuffer-diary-markdown "index.md")
(setq ibuffer-diary-preview "index.html")

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Dired"    (mode          . dired-mode))    ; Filter by mode
               ("Mail"     (or                                  ; Or multiple!
                            (mode . mew-summary-mode)
                            (mode . mew-message-mode)))
               ("Term"     (or                                  ; Or multiple!
                            (mode . vterm-mode)
                            (mode . term-mode)
                            (mode . shell-mode)
                            (mode . inferior-python-mode)
                            (mode . inferior-emacs-lisp-mode)))
               ("Calendar" (or                                  ; Or multiple!
                            (mode . calendar-mode)
                            (mode . cfw:calendar-mode)
                            (mode . cfw:details-mode)))
               ("Diary"    (or
                            (filename . "index.md")
                            (filename . "index.html")))
               ("Stars"    (starred-name))                  ; Group *starred*
               ("Unsaved"  (modified))                      ; All unsaved buffers
               ))))
;; 默认不展示空分组
(setq ibuffer-show-empty-filter-groups t)

;; Tell ibuffer to load the group automatically
(defun my-ibuffer-hook ()
  (ibuffer-switch-to-saved-filter-groups "default"))

(add-hook 'ibuffer-mode-hook 'my-ibuffer-hook)

;; 原来展示buffer list，现在展示ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; 阅读词汇
(defun pronounce-word()
  "Pronounce the word at point."
  (interactive)
  (let ((word (current-word)))
    (unless word
      (user-error "No word at point"))
    (message "word is '%s'" word)
    (cond 
     ((eq system-type 'darwin)  (call-process-shell-command (format "say %s" word)))
     (t (user-error (format "Unsupported OS: %s" system-type)))) ))
(global-set-key (kbd "C-c p") 'pronounce-word)


;; 保存一个窗口的位置，从而可以快速恢复
(defun save-windows-dispos (arg)
  (interactive "cSelect Register To Save:")
  (window-configuration-to-register arg)
  (message "Windows disposition saved to '%c'" arg))

(defun load-windows-dispos (arg)
  (interactive "cSelect Register To Load:")
  (jump-to-register arg)
  (message "Windows disposition loaded from '%c'" arg))

(global-set-key (kbd "<f10>") 'save-windows-dispos)              

(global-set-key (kbd "<f11>") 'load-windows-dispos)

;; 将一个窗口锁定，永远显示
(define-minor-mode locked-buffer-mode
  "Make the current window always display this buffer."
  :lighter " Locked"
  (set-window-dedicated-p (selected-window) locked-buffer-mode))

(global-set-key (kbd "<f12>") 'locked-buffer-mode)


(provide 'config-interface)
