;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mew扩展，用来管理邮箱列表并自动后台更新
;; 原始的Mew，只支持inbox自动更新
;; 这对于有多个imap邮箱中有多个目录的用户十分不方便
;; 特别是如果在server端做了邮件分拣的人更是如此
;;
;; File: mew-mbox-proto.el
;; Module:  协议扩展
;; Author:  sTeeL <steelm@madcat.cc>
;; Created: 2025/06/29
;; 
;;; Code:

(require 'mew-mbox-msg)

(defvar mew-mbox-proto-alist nil "支持的所有协议的列表")

(defun mew-mbox-register-proto (
                              proto
                              init
                              quit
                              status-update
                              ls-no-scan
                              update-mbox
                              update-all
                              generate-entries)
"注册一个协议，包含各种回调函数
proto: 协议名称(imap pop nntp local)
init:  初始化函数，在对应协议的mbox list被第一次以summary模式展示的时候调用
quit:  清理函数，在对应协议的buffer被关闭的时候调用
status-update: mew更新状态
ls-no-scan: mew切换summary视图的时候调用 (case mbox)
update-mbox: 更新特定mbox (case mbox &optional override-ma)
update-all: 更新所有mbox (case)
generate-entries: 生成条目(case no-zero-na)"
  (let ((entry (assoc proto mew-mbox-proto-alist)))
        (if (null entry)
            (setq mew-mbox-proto-alist
                  (cons
                   (cons
                    proto
                    (vector init quit status-update ls-no-scan update-mbox update-all generate-entries))
                   mew-mbox-proto-alist)))))
  

(defun mew-mbox-proto-call (proto func &rest args)
  "调用特定协议的特定函数"
  (mew-mbox-msg "proto-call \"%s\" %s %s" proto func args)
  (let* ((entry (assoc proto mew-mbox-proto-alist))
        (funcs (cdr entry)))
    (if (and proto entry funcs)
        (cond
         ((eq func 'init)(apply (aref funcs 0) args))
         ((eq func 'quit)(apply (aref funcs 1) args))
         ((eq func 'status-update)(apply (aref funcs 2) args))
         ((eq func 'ls-no-scan)(apply (aref funcs 3) args))
         ((eq func 'update-mbox)(apply (aref funcs 4) args))
         ((eq func 'update-all)(apply (aref funcs 5) args))
         ((eq func 'generate-entries)(apply (aref funcs 6) args)))
      )))


(defun mew-mbox-fld-proto-symbol (fld)
  "返回fld的协议symbol"
  (cond
   ((mew-folder-imapp fld) 'imap)
   ((mew-folder-popp fld) 'pop)
   ((mew-folder-nntpp fld) 'nntp)
   ((mew-folder-localp fld) 'local)
  ))

(defun mew-mbox-proto-symbol (proto)
  "proto前缀变为协议symbol，例如\"%\"->'imap"
  (cond
   ((string= "%" proto) 'imap)
   ((string= "$" protp) 'pop)
   ((string= "-" proto) 'nntp)
   ((string= "+") 'local)
  ))

(provide 'mew-mbox-proto)  
