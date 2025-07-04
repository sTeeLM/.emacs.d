;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mew扩展，用来管理邮箱列表并自动后台更新邮箱状态（是否有新邮件）
;; 原始的Mew，只支持inbox自动状态更新，并且所有邮箱都糊一起
;; 这对于有多个邮箱目录的用户十分不方便
;; 特别是如果在server端做了邮件分拣的人
;; 有非常多的邮件分拣规则，从而有超级多的邮箱目录
;;
;; File: mew-mbox-buffer.el
;; Module:  buffer元数据操作
;; Author:  sTeeLM <steelm@madcat.com>
;; Created: 2025/060/29
;; 
;;; Code:

(defvar mew-mbox-buffer-alist nil)

(defvar mew-mbox-buffer-find (case proto)
  "根据case和proto查找并返回buffer"
  nil)

(defun mew-mbox-buffer-get-property (buffer property)
  "获取一个buffer的property，proto case timer"
  (plist-get (alist-get buffer mew-mbox-buffer-alist) property))

(defun mew-mbox-buffer-set-property (buffer property value)
  "设置一个buffer的property，proto case timer, 如果没有buffer则创建之"
  (let ((entry (alist-get buffer mew-mbox-buffer-alist)))
    (when entry
      (assq-delete-all buffer mew-mbox-buffer-alist))
    (setq entry (plist-put entry property value))
    (push (cons buffer entry) mew-mbox-buffer-alist)))

(defun mew-mbox-buffer-is-member (buffer)
  "buffer 是在管理列表中吗？"
  (alist-get buffer mew-mbox-buffer-alist))

(defun mew-mbox-buffer-del-member (buffer)
  "从管理列表中删除一个buffer"
  (setq mew-mbox-buffer-alist (assq-delete-all buffer mew-mbox-buffer-alist)))

(defun mew-mbox-buffer-clear ()
  "清除整个元数据"
  (setq mew-mbox-buffer-alist nil))

(provide 'mew-mbox-buffer)
