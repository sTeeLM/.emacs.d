;; 设置 Mew
(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)

;;(add-to-list 'load-path "~/.emacs.d/mew-mbox")
;;(require 'mew-mbox)
;;(setq mew-mbox-verbose t)
;;(setq mew-mbox-imap-checker-proc "/Users/michael/mail-checker/imapcheck")

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
(setq mew-passwd-timer-unit 3600) ;1小时
(setq mew-passwd-lifetime 10) ; 10小时
(setq mew-passwd-reset-timer t)

;; 未读标记
(setq mew-use-unread-mark t)
;; 在所有邮箱中打开未读标记
(setq mew-unread-mark-list '((t t)))

;; 外观
(setq mew-summary-form
      '(type (5 year) (5 date) "|" (80 from) "|" t (0 subj)))

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
(setq mew-name "sTeeLM")
(setq mew-user "steelm")
(setq mew-mail-domain "madcat.cc")                                                                

(setq mew-ssl-verify-level 0)
;(setq mew-ssl-cert-directory "")
      
;; SMTP设置
(setq mew-smtp-user "steelm@madcat.cc")                                                             
(setq mew-smtp-server "smtp.madcat.cc")
(setq mew-smtp-ssl t)
(setq mew-smtp-ssl-port 465)
(setq mew-smtp-auth-list '("PLAIN" "LOGIN" "CRAM-MD5"))

;; IMAP设置
(setq mew-mailbox-type 'imap)
(setq mew-imap-size 0)
(setq mew-imap-delete nil) 
(setq mew-imap-user "steelm@madcat.cc")
(setq mew-imap-server "imap.madcat.cc")
(setq mew-imap-ssl t)
(setq mew-imap-ssl-port 993)
(setq mew-imap-auth nil)
(setq mew-imap-auth-list '("PLAIN" "CRAM-MD5"  "LOGIN"))
(setq mew-imap-header-only nil)
(setq mew-draft-folder "%Drafts")
(setq mew-imap-trash-folder "%Trash")
(setq mew-imap-trash-folder-list '("%Trash" "已删除邮件"))
(setq mew-fcc "%Sent")


;; 可以阅读html邮件
(require 'mime-w3m)
(setq mew-use-text/html t)
(setq mew-mime-multipart-alternative-list '("text/html" "text/plain" ".*"))
(setq mew-prog-html '(mew-mime-text/html mew-mime-text/html-ext))


;;;; HTML阅读器配置
;; (1) 使用内置的shr阅读HTML邮件，且不显示背景色
(setq shr-use-colors nil)

;; (2) 使用w3m阅读html邮件
;; (require 'w3m-load)
;; (require 'mew-w3m)
;; (setq mew-use-w3m-minor-mode t)
;; (setq mew-w3m-auto-insert-image t)
;; (add-hook 'mew-message-hook 'mew-w3m-minor-mode-setter)
;; (setq mew-prog-text/html 'mew-mime-text/html-w3m)
;; (setq mew-prog-text/html-ext 'mew-mime-text/html-w3m)
;; (define-key mew-summary-mode-map "T" 'mew-w3m-view-inline-image)

;;mew启动时不自动获取邮件
(setq mew-auto-get nil)

;; 设置使用Biff检查邮箱是否有新邮件，默认为5分钟。如果有新邮件，则在emacs的状态栏显示Mail(n)的提示—n表示新邮件数目。
(setq mew-use-biff t)

;; 设置嘟嘟声通知有新邮件
(setq mew-use-biff-bell t)

;; 设置自动检查新邮件的时间间隔，单位：分钟
(setq mew-biff-interval 5)


;; 回复和签名档设置
(setq mew-signature-file "~/.emacs.d/signature")
(setq mew-cite-fields '("Date:"  "From:"))
(setq mew-cite-format "On %s %s wrote:\n\n")
(setq mew-signature-as-lastpart t)
(setq mew-signature-insert-last t)
(add-hook 'mew-before-cite-hook 'mew-header-goto-body)
(add-hook 'mew-draft-mode-newdraft-hook 'mew-draft-insert-signature)

;; 加密和签名
;;(setq mew-pgp-ascii-suffix "84599A3C")
;;(setq mew-draft-privacy-method 'smime)
;;(setq mew-protect-privacy-always t)
;;(setq mew-protect-privacy-always-type 'smime-signature)


(provide 'config-mail)
