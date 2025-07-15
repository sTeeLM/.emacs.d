;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mew扩展，用来管理邮箱列表并自动后台更新
;; 原始的Mew，只支持inbox自动更新
;; 这对于有多个imap邮箱中有多个目录的用户十分不方便
;; 特别是如果在server端做了邮件分拣的人更是如此
;;
;; File: mew-mail-sum-msg.el
;; Module:  消息输出
;; Author:  sTeeL <steel.mental@gmail.com>
;; Created: 2015/10/07 15:52:15
;; 
;;; Code:

(require 'config-lib)

(sm-log-make-logger-level "mew-mbox")
(sm-log-make-logger-debug "mew-mbox" "*MML Messages*")
(sm-log-make-logger-info  "mew-mbox" "*MML Messages*")
(sm-log-make-logger-warn  "mew-mbox" "*MML Messages*")
(sm-log-make-logger-error "mew-mbox" "*MML Messages*")


(provide 'mew-mbox-msg)
