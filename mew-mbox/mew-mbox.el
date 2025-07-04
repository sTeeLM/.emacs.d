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
(require 'mew-mbox-buffer)
(require 'mew-mbox-proto)
(require 'mew-mbox-msg)
(require 'tabulated-list)

(require 'mew-mbox-imap)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom mew-mbox-refresh-interval 1
  "*主界面刷新间隔（秒）."
  :group 'mew-env
  :type 'integer)

(defconst mew-mbox-buffer-name-template "*Mailbox List [%s][%s]*")

(defun mew-mbox-buffer-name (case proto)
  (format mew-mbox-buffer-name-template (or case "default") proto))

(defvar mew-mbox-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map) map))

(define-key mew-mbox-mode-map "v" 'mew-mbox-view-mbox)
(define-key mew-mbox-mode-map (kbd "RET") 'mew-mbox-view-mbox)
(define-key mew-mbox-mode-map "u" 'mew-mbox-update-mbox)
(define-key mew-mbox-mode-map "U" 'mew-mbox-update-all)
(define-key mew-mbox-mode-map "r" 'mew-mbox-refresh)
(define-key mew-mbox-mode-map "f" 'mew-mbox-filter-zero-na)
(define-key mew-mbox-mode-map "Q" 'mew-summary-quit)

;; 在summary模式下按G键打开邮箱列表
(define-key mew-summary-mode-map "G"    'mew-mbox-open-buffer)

(defvar mew-mbox-filter-zero-na nil)

(define-derived-mode mew-mbox-mode tabulated-list-mode "Mew MailBox List"
  (add-hook 'tabulated-list-revert-hook 'mew-mbox-buffers-revert nil t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mew-mbox-refresh-func (buffer)
  "定时刷新主页面，如果buffer被关闭，停止timer。因为主页面上的mbox的信息可能会定时更新。"
  (mew-mbox-msg "mew-mbox-refresh-func called for buffer %s" buffer)
  (when buffer
    (with-current-buffer buffer
      (mew-mbox-refresh))))


(defun mew-mbox-create-refresh-timer (buffer)
  "创建主界面更新定时器"
  (let ((timer (run-with-idle-timer  mew-mbox-refresh-interval t 'mew-mbox-refresh-func buffer)))
    (mew-mbox-msg "timer %s for buffer %s created" timer buffer)
    timer))

(defun mew-mbox-open-buffer ()
  "打开当前case下的主界面"
  (interactive)
  (mew-mbox-msg "mew-mbox-open-buffer called")
  (let* ((buffer-name (mew-mbox-buffer-name mew-case mew-proto))
         (buffer (get-buffer buffer-name)))
    (unless buffer
      (setq buffer (generate-new-buffer buffer-name))      
      (with-current-buffer buffer
        (mew-mbox-mode)
        (mew-mbox-recreate-table)
        (tabulated-list-print))
      (mew-mbox-proto-call (mew-mbox-proto-symbol mew-proto) 'init buffer)
      (mew-mbox-buffer-set-property buffer 'proto mew-proto)
      (mew-mbox-buffer-set-property buffer 'case  (or mew-case "default"))
      (mew-mbox-buffer-set-property buffer 'refresh-timer (mew-mbox-create-refresh-timer buffer)))
    (display-buffer buffer)))

(defun mew-mbox-close-buffer ()
  "在主界面关闭前进行清理"
  (let ((buffer (current-buffer)))
    (when (mew-mbox-buffer-is-member buffer)
      (let ((proto (mew-mbox-buffer-get-property buffer 'proto))
            (timer (mew-mbox-buffer-get-property buffer 'refresh-timer)))
        (when timer
          (mew-mbox-msg "timer %s for buffer %s canceled" timer buffer)
          (cancel-timer timer))
        (when proto
          (mew-mbox-proto-call (mew-mbox-proto-symbol proto) 'quit buffer))
        (mew-mbox-buffer-del-member buffer)))))

(add-hook 'kill-buffer-hook 'mew-mbox-close-buffer)

(defun mew-mbox-view-mbox()
  "访问界面上选中的mbox"
  (interactive)
  (let* ((id (tabulated-list-get-id))
         case:folder)
    (if id
        (progn 
          (setq case:folder (concat (car id) ":" (cadr id)))
          (mew-summary-visit-folder case:folder t)))))

(defun mew-box-update-mbox()
  "更新界面上选中的mbox"
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (case (car id))
         (mbox (cadr id))
         proto)
    (when mbox
      (setq proto (mew-mbox-fld-proto-symbol mbox))
      (mew-mbox-proto-call
       (mew-mbox-proto-test-proto proto)
       'update-mbox (current-buffer) mbox))))

(defun mew-mbox-update-all()
  "更新所有mbox"
  (interactive)
  (mew-mbox-proto-call 'update-all-mbox (current-buffer)))

(defun mew-mbox-refresh ()
  "更新界面"
  (interactive)
  (mew-mbox-recreate-table)
  (tabulated-list-print t))

(defun mew-mbox-filter-zero-na ()
  "toggle 是否显示空mbox"
  (interactive)
  (setq mew-mbox-filter-zero-na (not mew-mbox-filter-zero-na))
  (mew-mbox-refresh))

(defun mew-mbox-generate-entries ()
  "生成表格条目"
  ;;(mew-mbox-msg "mew-mbox-generate-entries called")
  (let (
        (no-zero-na mew-mbox-filter-zero-na)
        (entries))
    (setq entries (mew-mbox-proto-call
                   (mew-mbox-proto-symbol mew-proto)
                   'generate-entries (current-buffer) no-zero-na))
    entries))

(defun mew-mbox-sort-mail (el1 el2)
  (let* ((val1 (cadr el1))
         (val2 (cadr el2))
         (mail1 (string-to-number (aref val1 3)))
         (mail2 (string-to-number (aref val2 3)))
         )
    (<= mail1 mail2)))

(defun mew-mbox-recreate-table ()
  "重新生成表格界面"
  (setq tabulated-list-format
	    (vector
         ;; 更新中显示*
         '("U" 1 t :pad-right 0)
         ;; 协议
         '("P" 2 t :pad-right 0)
         ;; 最后更新时间
		 '("LAST-UPDATE" 20 t :pad-right 0 :left-align t)
         ;; 总邮件数
         '("MAIL-TOTAL" 10 mew-mbox-sort-mail :right-align t)
         ;; 未读邮件数
         '("MAIL-NEW" 10 mew-mbox-sort-mail :right-align t)
         ;; 配置名
         '("CASE" 8 t)
         ;; 邮箱名
		 '("MBOX" 8 t)))
  (setq tabulated-list-use-header-line t)
  (setq tabulated-list-entries 'mew-mbox-generate-entries)
  (tabulated-list-init-header))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mew-mbox-status-update ()
  (let ((buffer (mew-mbox-find-buffer mew-case mew-proto)))
    (when buffer
      (mew-mbox-proto-call (mew-mbox-proto-symbol mew-proto) 'status-update buffer mew-init-p))))

(defun mew-mbox-init ()
  "初始化函数, 在启动mew的时候调用"
  (mew-mbox-msg "mew-mbox-init called"))


(defun mew-mbox-quit()
  "清理函数，在mew推出时调用，关闭所有buffer"
  (mew-mbox-msg "mew-mbox-quit called")
  (dolist (entry mew-mbox-buffer-alist)
    (kill-buffer (car entry)))
  (mew-mbox-buffer-clear))


(defun mew-mbox-buffers-revert ()
  (mew-mbox-msg "mew-mail-sum-buffers-revert called"))

(add-hook 'mew-status-update-hook 'mew-mbox-status-update)
(add-hook 'mew-init-hook 'mew-mbox-init)
(add-hook 'mew-quit-hook 'mew-mbox-quit)

(provide 'mew-mbox)
