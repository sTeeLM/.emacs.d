;; 设置 Mew
(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)

;; Optional setup (Read Mail menu for Emacs 21):
(if (boundp 'read-mail-command)
    (setq read-mail-command 'mew))

;; Optional setup (e.g. C-xm for sending a message):
(autoload 'mew-user-agent-compose "mew" nil t)
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'mew-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'mew-user-agent
      'mew-user-agent-compose
      'mew-draft-send-message
      'mew-draft-kill
      'mew-send-hook))

;; 下面的图标路径和安装路径有关，具体请看Mew的安装过程
(setq mew-icon-directory "/opt/local/share/emacs/site-lisp/mew/etc")

;; 不用频繁输入密码
(setq mew-use-cached-passwd t)

;; 未读标记
(setq mew-use-unread-mark t)
(setq mew-unread-mark-list '((t t)))

;; 外观
;;(setq mew-summary-form
;;      '(type (5 date) " " (14 from) " " t (30 subj) "|" (0 body)))
(setq mew-sort-default-key "date")
(setq mew-window-use-full t)
(setq mew-underline-lines-use t)
(setq mew-use-fancy-thread t)
(setq mew-use-fancy-highlight-body t)
(setq mew-fancy-highlight-body-prefix-width 10)
(setq mew-highlight-body-regex-comment "^[;#?%]+.*")
(setq mew-prog-imls-arg-list '("--thread=yes" "--indent=2"))

;; 邮箱设置
(setq mew-proto "%")
(setq mew-use-unread-mark t)

;; 默认发件人设置
(setq mew-name "李闻")
(setq mew-user "liwen03")
(setq mew-mail-domain "meituan.com")                                                                

;; SMTP设置
(setq mew-smtp-user "michael")                                                             
(setq mew-smtp-server "mail.steel.hell.com")                                                           
(setq mew-smtp-auth-list '("PLAIN" "LOGIN" "CRAM-MD5"))

;; IMAP设置
(setq mew-imap-size 0)
(setq mew-imap-delete nil) 
(setq mew-imap-user "michael")
(setq mew-imap-server "mail.steel.hell.com")
(setq mew-imap-auth-list '("PLAIN" "LOGIN" "CRAM-MD5")) 

(setq mew-draft-folder "%Drafts")
(setq mew-imap-trash-folder "%Deleted Messages")
(setq mew-fcc "%Sent")


;; 可以阅读html邮件
(require 'mew-w3m)
(setq mew-use-text/html t)
(setq mew-mime-multipart-alternative-list '("text/html" "text/plain" ".*"))
(setq mew-use-w3m-minor-mode t)
(setq mew-w3m-auto-insert-image t)

;;mew启动时自动获取邮件
(setq mew-auto-get nil)

;; 设置使用Biff检查邮箱是否有新邮件，默认为5分钟。如果有新邮件，则在emacs的状态栏显示Mail(n)的提示—n表示新邮件数目。
(setq mew-use-biff t)

;; 设置嘟嘟声通知有新邮件
(setq mew-use-biff-bell t)

;; 设置自动检查新邮件的时间间隔，单位：分钟
(setq mew-biff-interval 5)

(provide 'config-mew)
