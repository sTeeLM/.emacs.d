;设置打开文件的缺省路径
(setq default-directory "~/")

;;关闭烦人的出错时的提示声
(setq visible-bell t)

;;关闭emacs启动时的画面
(setq inhibit-startup-message t)

;;关闭gnus启动时的画面
(setq gnus-inhibit-startup-message t)

;; 改变 Emacs 固执的要你回答 yes 的行为。按 y 或空格键表示 yes，n 表示 no。
(fset 'yes-or-no-p 'y-or-n-p)

;;设置粘贴缓冲条目数量.用一个很大的kill ring(最多的记录个数). 这样防止我不小心删掉重要的东西
(setq kill-ring-max 2000)

;; Autofill in all modes;;
(setq-default auto-fill-function 'do-auto-fill)

;;把 fill-column 设为 60. 这样的文字更好读
(setq default-fill-column 120)

;;不用 TAB 字符来indent, 这会引起很多奇怪的错误。编辑 Makefile 的时候也不用担心，因为 makefile-mode 会把 TAB 键设置成真正的 TAB 字符，并且加亮显示的。
(setq-default indent-tabs-mode nil)
(setq default-tab-width 8);;tab键为8个字符宽度
(setq tab-stop-list ())

;;设置 sentence-end 可以识别中文标点。不用在 fill 时在句号后插入两个空格。
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

;;可以递归的使用 minibuffer
(setq enable-recursive-minibuffers t)

;;防止页面滚动时跳动， scroll-margin 3 可以在靠近屏幕边沿3行时就开始滚动，可以很好的看到上下文。
(setq scroll-margin 3 scroll-conservatively 10000)

;;设置缺省主模式是text，,并进入auto-fill次模式.而不是基本模式fundamental-mode
(setq default-major-mode 'text-mode)
;;(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; 使用xclip-mode，打通系统剪切版
(require 'xclip)
(add-hook 'text-mode-hook 'xclip-mode)

;;打开括号匹配显示模式
(show-paren-mode t)

;;括号匹配时可以高亮显示另外一边的括号，但光标不会烦人的跳到另一个括号处。
(setq show-paren-style 'parenthesis)

;; 当有两个文件名相同的缓冲时，使用前缀的目录名做 buffer 名字，不用原来的foobar 形式。
(setq uniquify-buffer-name-style 'forward);;好像没起作用

;;让 Emacs 可以直接打开和显示图片。
(setq auto-image-file-mode t)

;打开压缩文件时自动解压缩。
(auto-compression-mode 1)

;; 在行首 C-k 时，同时删除该行。
(setq-default kill-whole-line t)

;;当你在shell、telnet、w3m等模式下时，必然碰到过要输入密码的情况,此时加密显出你的密码
(add-hook 'comint-output-filter-functions
      'comint-watch-for-password-prompt)

;; 设定不产生备份文件
(setq make-backup-files nil)

;; Emacs 中，改变文件时，默认都会产生备份文件(以 ~ 结尾的文件)。可以完全去掉;
;; (并不可取)，也可以制定备份的方式。这里采用的是，把所有的文件备份都放在一个固定的地方("~/backups")。
;; 对于每个备份文件，保留最原始的两个版本和最新的1个版本。并且备份的时候，备份文件是复本，而不是原件。
;; (setq version-control t);;启用版本控制，即可以备份多次
;; (setq kept-old-versions 2);;备份最原始的版本两次，及第一次编辑前的文档，和第二次编辑前的文档
;; (setq kept-new-versions 1);;备份最新的版本1次，理解同上
;; (setq delete-old-versions t);;删掉不属于以上3中版本的版本
;; (setq backup-directory-alist '(("." . "~/backups")));;设置备份文件的路径
;; (setq backup-by-copying t);;备份设置方法，直接拷贝

;;自动保存模式
(setq auto-save-mode nil)

;; 不生成临时文件
(setq-default make-backup-files nil)

;;把这些缺省禁用的功能打开。
(put 'scroll-left 'disabled nil) ;允许屏幕左移
(put 'scroll-right 'disabled nil) ;允许屏幕右移
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'LaTeX-hide-environment 'disabled nil)

(setq x-select-enable-clipboard t)
;;允许emacs和外部其他程序的粘贴

;;使用鼠标中键可以粘贴
;;(setq mouse-yank-at-point t)


;;设置有用的个人信息,这在很多地方有用。
(setq user-full-name "steelm")
(setq user-mail-address "steelm@madcat.cc")

;; 自动的在文件末增加一新行
(setq require-final-newline nil)

;;Non-nil if Transient-Mark mode is enabled.
(setq-default transient-mark-mode t)

;; 当光标在行尾上下移动的时候，始终保持在行尾。
(setq track-eol t)

;; 当浏览 man page 时，直接跳转到 man buffer。
(setq man-notify-method 'pushy)


;;Emacs 21 中已经是缺省设置。按 C-n 或向下键时不添加新行。
(setq next-line-add-newlines nil)

;;ido的配置,这个可以使你在用C-x C-f打开文件的时候在后面有提示;
;;这里是直接打开了ido的支持，在emacs23中这个是自带的.
(require 'ido)
(ido-mode t)

;;ido模式中不保存目录列表,解决退出Emacs时ido要询问编码的问题。
(setq ido-save-directory-list-file nil)

;; C-x C-d 不调用ido-list-directory而是调用dired
(require 'dired)
(global-set-key (kbd "C-x C-d") 'ido-dired)

;; 不自动保存打开文件
(desktop-save-mode 0)

;; 在term里运行一个进程
(defun my-term-handle-exit (&optional process-name msg)
  (message "%s quit: %s" process-name (replace-regexp-in-string "\n$" "" msg))
  (kill-buffer (current-buffer)))

(advice-add 'term-handle-exit :after 'my-term-handle-exit)

(defun term-run-command (shell-command-string buffer-name &optional show)
  "在term里运行一个shell命令"
  (if (require 'term nil t)
      (let ((prog (split-string-shell-command shell-command-string)))
        (save-current-buffer
         (set-buffer (apply #'make-term buffer-name (car prog) nil (cdr prog)))
         (term-char-mode))
        (when show
          (pop-to-buffer-same-window (format "*%s*" buffer-name))))
    (error "term is not present or not loaded on this version of Emacs")))

;; custom-file，不要把custom-set-variables胡乱写到我的配置文件里
(setq custom-file "~/.emacs.d/custom-file.el")

(provide 'config-system)
