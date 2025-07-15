;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mew扩展，用来管理邮箱列表并自动后台更新邮箱状态（是否有新邮件）
;; 原始的Mew，只支持inbox自动状态更新，并且所有邮箱都糊一起
;; 这对于有多个邮箱目录的用户十分不方便
;; 特别是如果在server端做了邮件分拣的人
;; 有非常多的邮件分拣规则，从而有超级多的邮箱目录
;;
;; File: mew-mbox.el
;; Module:  主模块
;; Author:  sTeeLM <steelm@madcat.com>
;; Created: 2025/060/29
;; 
;;; Code:

(require 'mew)
(require 'mew-mbox-msg)
(require 'mew-mbox-buffer)
(require 'mew-mbox-proto)
(require 'tabulated-list)
(require 'mew-mbox-imap)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom mew-mbox-refresh-interval 5
  "*主界面刷新间隔（秒）."
  :group 'mew-env
  :type 'integer)

(defcustom mew-mbox-filter-regex-list nil
  "哪些不应该被展示的邮箱名称的regex，使用string-match匹配"
  :group 'mew-env)

(defcustom mew-mbox-open-on-init t
  "是否在mew启动的时候就打开"
  :group 'mew-env
  :type 'boolean)

(defcustom mew-mbox-use-biff t
  "是否在状态栏上显示邮件"
  :group 'mew-env
  :type 'boolean)

(defcustom mew-mbox-biff-fun 'mew-mbox-biff-fun-default
  "在biff的时候，调用的函数"
  :group 'mew-env
  :type 'function)

(defcustom mew-mbox-biff-new-mail-sound-fun nil
  "新邮件到来的时候，调用的播放声音的函数"
  :group 'mew-env
  :type 'function)  

(defvar mew-mbox-biff-saved-mail-cnt 0
  "上一次看到的邮件数")

(defconst mew-mbox-buffer-name-template "*Mailbox List [%s][%s]*")

(defvar mew-mbox-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map) map))

(defvar mew-mbox-filter-name-do nil
  "是否应用mew-mbox-filter-regex-list")

(defvar mew-mbox-filter-zero-do nil
  "是否过滤new-mail为0的邮箱")

(defvar mew-mbox-biff-string nil
  "biff时候，显示扎状态栏上的字符串")


(defface mew-mbox-new-mail-face
  '((t (:inherit bold :foreground "cyan")))
  "新邮件使用的face")

(defvar mew-mbox-saved-window-config nil
  "保存下来的window配置，用来快速跳回mbox列表")


(define-key mew-mbox-mode-map "v" 'mew-mbox-view-mbox-and-ls)
(define-key mew-mbox-mode-map (kbd "RET") 'mew-mbox-view-mbox)
(define-key mew-mbox-mode-map "s" 'mew-mbox-update-mbox)
(define-key mew-mbox-mode-map "S" 'mew-mbox-update-all)
(define-key mew-mbox-mode-map "r" 'mew-mbox-refresh)
(define-key mew-mbox-mode-map "f" 'mew-mbox-toggle-filter-name)
(define-key mew-mbox-mode-map "0" 'mew-mbox-toggle-filter-new)
(define-key mew-mbox-mode-map "Q" 'mew-summary-quit)

;; 在summary模式下按G键打开邮箱列表
(define-key mew-summary-mode-map "G"    'mew-mbox-open-buffer)
(define-key mew-message-mode-map "q"    'kill-buffer-and-window)

(defun mew-mbox-buffers-revert ()
  (mew-mbox-log-debug "mew-mail-sum-buffers-revert called"))

(define-derived-mode mew-mbox-mode tabulated-list-mode "Mew MailBox List"
  (add-hook 'tabulated-list-revert-hook 'mew-mbox-buffers-revert nil t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mew-mbox-biff-fun-default (n)
  "默认的biff函数"
  (when (> n mew-mbox-biff-saved-mail-cnt)
    (when (functionp mew-mbox-biff-new-mail-sound-fun)
      (funcall mew-mbox-biff-new-mail-sound-fun)))
  (setq mew-mbox-biff-saved-mail-cnt n)
  (if (= n 0)
      (setq mew-mbox-biff-string nil)
    (setq mew-mbox-biff-string (format "📧 New Mail[%d] " n))))

(defun mew-mbox-biff-setup ()
  "初始化biff功能"
  (when mew-mbox-use-biff
    (let ((ent '(mew-mbox-biff-string mew-mbox-biff-string)))
      (unless (member ent global-mode-string)
        (if global-mode-string
            (setq global-mode-string
                  (append global-mode-string (list "" ent)))
          (setq global-mode-string (list ent)))))))
  

(defun mew-mbox-biff-clean ()
  "清除biff功能"
  (setq mew-mbox-biff-string nil))
  
(defun mew-mbox-calculate-biff ()
  "查看所有buffer的biff-cnt属性并累加，然后调用mew-mbox-biff-fun"
  (when (functionp mew-mbox-biff-fun)
    (funcall mew-mbox-biff-fun (mew-mbox-buffer-count-biff))))

(defun mew-mbox-buffer-name (case proto)
  (format mew-mbox-buffer-name-template (or case "default") proto))

(defun mew-mbox-refresh-func (buffer)
  "定时刷新主页面，如果buffer被关闭，停止timer。因为主页面上的mbox的信息可能会定时更新。"
  (mew-mbox-log-debug "mew-mbox-refresh-func called for buffer %s" buffer)
  (when buffer
    (mew-mbox-create-refresh-timer buffer)
    (with-current-buffer buffer (mew-mbox-refresh))))


(defun mew-mbox-create-refresh-timer (buffer)
  "创建主界面更新定时器"
  (mew-mbox-buffer-set-property buffer 'refresh-timer
                                (run-with-timer
                                 mew-mbox-refresh-interval
                                 nil
                                 'mew-mbox-refresh-func buffer)))

(defun mew-mbox-open-buffer ()
  "打开当前case下的主界面"
  (interactive)
  (mew-mbox-log-debug "mew-mbox-open-buffer called")
  (let* ((buffer-name (mew-mbox-buffer-name (or mew-case "default") mew-proto))
         (buffer (get-buffer buffer-name)))
    (unless buffer
      (setq buffer (generate-new-buffer buffer-name))      
      (with-current-buffer buffer
        (mew-mbox-proto-call (mew-mbox-proto-symbol mew-proto) 'init buffer)
        (mew-mbox-buffer-set-property buffer 'proto mew-proto)
        (mew-mbox-buffer-set-property buffer 'case  (or mew-case "default"))
        (mew-mbox-create-refresh-timer buffer)
        (mew-mbox-buffer-set-property buffer 'biff-cnt 0)
        (mew-mbox-mode)
        (mew-mbox-recreate-table)
        (tabulated-list-print)))
    (pop-to-buffer buffer)))

(defun mew-mbox-close-buffer ()
  "在主界面关闭前进行清理"
  (let ((buffer (current-buffer)))
    (when (mew-mbox-buffer-is-member buffer)
      (let ((proto (mew-mbox-buffer-get-property buffer 'proto))
            (timer (mew-mbox-buffer-get-property buffer 'refresh-timer)))
        (when timer
          (mew-mbox-log-debug "timer %s for buffer %s canceled" timer buffer)
          (cancel-timer timer))
        (when proto
          (mew-mbox-proto-call (mew-mbox-proto-symbol proto) 'quit buffer))
        (mew-mbox-buffer-del-member buffer)))))

(add-hook 'kill-buffer-hook 'mew-mbox-close-buffer)

(defun mew-mbox-view-mbox()
  "访问界面上选中的mbox"
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (case:folder))
    (when id
      (setq mew-mbox-saved-window-config (current-window-configuration))
      (setq case:folder (mew-case-folder (car id) (cadr id)))
      (mew-summary-visit-folder case:folder t))))

(defun mew-mbox-view-mbox-and-ls ()
  "访问选中的mbox，同步，并且触发单次更新"
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (case (car id))
         (mbox (cadr id))
         (case:folder))
    (when id
      (setq mew-mbox-saved-window-config (current-window-configuration))
      (setq case:folder (mew-case-folder case mbox))
      (mew-summary-visit-folder case:folder t)
      (mew-summary-ls nil t t))))
      

(defun mew-mbox-update-mbox()
  "更新界面上选中的mbox"
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (case (car id))
         (mbox (cadr id))
         (proto (mew-mbox-fld-proto-symbol mbox)))
    (when mbox
      (when (mew-mbox-proto-call
             proto 'update-mbox (current-buffer) mbox)
        (message "Success start update process for %s" mbox)))))

(defun mew-mbox-update-all()
  "更新所有mbox"
  (interactive)
  (let* ((buffer (current-buffer))
        (proto (mew-mbox-buffer-get-property buffer 'proto)))
    (when (mew-mbox-proto-call (mew-mbox-proto-symbol proto) 'update-all buffer)
      (message "Success start batch update process group"))))

(defun mew-mbox-refresh ()
  "更新界面"
  (interactive)
  (mew-mbox-calculate-biff)
  (mew-mbox-recreate-table)
  (tabulated-list-print t))

(defun mew-mbox-toggle-filter-name ()
  "toggle 是否显示空mbox"
  (interactive)
  (setq mew-mbox-filter-name-do (not mew-mbox-filter-name-do))
  (mew-mbox-refresh))

(defun mew-mbox-toggle-filter-new ()
  "toggle 是否显示new-mail为0的条目"
  (interactive)
  (setq mew-mbox-filter-zero-do (not mew-mbox-filter-zero-do))
  (mew-mbox-refresh))

(defun mew-mbox-generate-entries ()
  "生成表格条目"
  ;;(mew-mbox-log-debug "mew-mbox-generate-entries called")
  (let ((filter-name (if mew-mbox-filter-name-do mew-mbox-filter-regex-list nil))
        (entries))
    (setq entries (mew-mbox-proto-call
                   (mew-mbox-proto-symbol mew-proto)
                   'generate-entries (current-buffer) filter-name mew-mbox-filter-zero-do))
    entries))


(defun mew-mbox-sort-mail-index (el1 el2 index)
  (let* ((val1 (cadr el1))
         (val2 (cadr el2))
         (mail1 (string-to-number (aref val1 index)))
         (mail2 (string-to-number (aref val2 index))))
    (<= mail1 mail2)))

(defun mew-mbox-sort-mail-new (el1 el2)
  (mew-mbox-sort-mail-index el1 el2 3))

(defun mew-mbox-sort-mail-remote (el1 el2)
  (mew-mbox-sort-mail-index el1 el2 4))

(defun mew-mbox-sort-mail-local (el1 el2)
  (mew-mbox-sort-mail-index el1 el2 5))

(defun mew-mbox-sort-mail-unseen (el1 el2)
  (mew-mbox-sort-mail-index el1 el2 6))

(defun mew-mbox-sort-mail-uidnext (el1 el2)
  (mew-mbox-sort-mail-index el1 el2 7))

(defun mew-mbox-sort-mail-msg-id (el1 el2)
  (mew-mbox-sort-mail-index el1 el2 8))


(defun mew-mbox-recreate-table ()
  "重新生成表格界面"
  (setq tabulated-list-format
	    (vector
         ;; 更新中显示*
         '("U" 2 nil :pad-right 0)
         ;; 邮箱名
		 '("MBOX" 30 t)
         ;; 最后更新时间
		 '("LAST-UPDATE" 30 t :left-align t)
         ;; 未读邮件数
         '("MAIL-NEW" 12 mew-mbox-sort-mail-new :left-align t)
         ;; 远程邮件数
         '("MAIL-REMOTE" 12 mew-mbox-sort-mail-remote :left-align t)
         ;; 本地邮件数
         '("MAIL-LOCAL" 12 mew-mbox-sort-mail-local :left-align t)
         ;; 远程邮箱中的UNSEEN数
         '("UNSEEN" 12 mew-mbox-sort-mail-unseen :left-align t)
         ;; UIDNEXT
         '("UIDNEXT" 12 mew-mbox-sort-mail-uidnext :left-align t)
         ;; MSG-ID
         '("MSG-ID" 12 mew-mbox-sort-mail-msg-id :left-align t)
         ))
  (setq tabulated-list-use-header-line t)
  (setq tabulated-list-entries 'mew-mbox-generate-entries)
  (tabulated-list-init-header))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mew-mbox-status-update ()
  (let ((buffer (mew-mbox-buffer-find (or mew-case "default") mew-proto)))
    (when buffer
      (mew-mbox-proto-call (mew-mbox-proto-symbol mew-proto) 'status-update buffer mew-init-p))))

(defun mew-mbox-init ()
  "初始化函数, 在启动mew的时候调用"
  (mew-mbox-log-debug "mew-mbox-init called")
  (mew-mbox-biff-setup)
  (when mew-mbox-open-on-init
    (mew-mbox-open-buffer)))


(defun mew-mbox-quit()
  "清理函数，在mew推出时调用，关闭所有buffer"
  (mew-mbox-log-debug "mew-mbox-quit called")
  (dolist (entry mew-mbox-buffer-alist)
    (kill-buffer (car entry)))
  (mew-mbox-biff-clean) 
  (mew-mbox-buffer-clear))

(defun mew-mbox-update-mbox()
  "更新界面上选中的mbox"
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (case (car id))
         (mbox (cadr id))
         proto)
    (when mbox
      (setq proto (mew-mbox-fld-proto-symbol mbox))
      (mew-mbox-proto-call
       proto 'update-mbox (current-buffer) mbox))))

(defun mew-mbox-suspend()
  (when mew-mbox-saved-window-config
    (set-window-configuration mew-mbox-saved-window-config)))

;;; 添加一些hook
(add-hook 'mew-status-update-hook 'mew-mbox-status-update)
(add-hook 'mew-init-hook 'mew-mbox-init)
(add-hook 'mew-quit-hook 'mew-mbox-quit)
(add-hook 'mew-suspend-hook 'mew-mbox-suspend)
(provide 'mew-mbox)
