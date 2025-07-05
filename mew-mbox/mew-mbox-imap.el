;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File: mew-mbix-imap.el
;; Module:  IMAP协议支持
;; Author:  sTeeL <steelm@madcat.cc>
;; Created: 2015/10/07 15:52:15
;; 
;;; Code:

(require 'mew)
(require 'mew-mbox-msg)
(require 'mew-mbox-proto)
(require 'mew-mbox-buffer)

(defcustom mew-mbox-imap-checker-proc "imapcheck"
  "*imap checker程序名."
  :group 'mew-env
  :type 'file)

(defcustom mew-mbox-imap-checker-max-background-count 10
  "最多同时运行的异步后台imap checker数"
  :group 'mew-env
  :type 'integer)

(defcustom mew-mbox-imap-checker-interval 300
  "两轮imap checker扫描的时间间隔（秒）"
  :group 'mew-env
  :type 'integer)

(defconst mew-mbox-imap-cache-file ".mew-mbox-cache-imap")
(defconst mew-mbox-imap-process-name "mew-mbox-imap-checker")
(defconst mew-mbox-imap-checker-prefix "mew-mbox-imap-checker-")

;; 元数据格式：
;;((mbox-name . [INDEX TIMESTAMP MSGID MESSAGES RECENT UIDNEXT UIDVALIDITY UNSEEN]) ... )
(defun mew-mbox-imap-alist-load (case force-rebuild)
  "装载mbox元数据，返回alist 或者 nil，优先读取cache文件，\
如果没有cache文件，直接重新构建一个。\
如果force-rebuild为t，强制重新构建"
  (let* ((alist) (mbox-list) (mbox-list-slot) (index 0)
         (mbox-path (mew-expand-folder (mew-case-folder case "%")))
         (mbox-alist-file-path (file-name-concat mbox-path mew-mbox-imap-cache-file)))
    (condition-case err 
        (if (or force-rebuild (not (file-exists-p  mbox-alist-file-path)))
            (progn 
              (setq mbox-list (mew-imap-folder-alist case))
              (dolist (mbox-list-slot mbox-list)
                (setq alist (append alist (list (car mbox-list-slot)
                                                (vector index nil nil nil nil nil nil))))
                (setq index (1+ index))))
          (progn
            (setq alist (mew-lisp-load mbox-alist-file-path))))
      (error (message "mew-mbox-imap-alist-load failed %s" err)))
    alist))

(defun mew-mbox-imap-alist-save (case alist)
  "保存mbox元数据"
  (let* ((mbox-path (mew-expand-folder (mew-case-folder case "%")))
         (mbox-alist-file-path (file-name-concat mbox-path mew-mbox-imap-cache-file)))
    (condition-case err
        (mew-lisp-save mbox-alist-file-path alist t t)
      (error (message "mew-mbox-imap-alist-save failed %s" err)))))

(defun mew-mbox-imap-alist-mail-new (msgid messages uidnext)
  "计算有多少邮件是未读的"
  (let ((ret 0))
    (if (or (equal 0 messages) (>= msgid messages))
        (setq ret 0) ;; 显示为0
      (progn
        (if (equal 0 msgid) ;; 如果msgid == 0
            (setq ret messages) ;; 显示messages
          (setq ret (- (- uidnext msgid) 1))) ;; 否则计算uidnext和msgid的差距
        )
      )
    ret))

(defun mew-mbox-imap-alist-is-updating (mbox plist)
  "mbox 是否正在更新？本质是检查mbox是否在plist里"
  (if (assoc mbox plist) t nil))

(defun mew-mbox-imap-alist-export(case alist plist)
  "export mbox的元数据到list的entry数据"
  (let ((entries) (alist-slot))
    (dolist (alist-slot alist)
      (let* (
             (mbox (car alist-slot))
             (timestamp (mew-mbox-imap-alist-get alist mbox 'TIMESTAMP))
             (msgid (mew-mbox-imap-alist-get alist mbox 'MSGID))
             (messages (mew-mbox-imap-alist-get alist mbox 'MESSAGES))
             (uidnext (mew-mbox-imap-alist-get alist mbox 'UIDNEXT))
             )
        (setq entries (append alist (list
                                     ;; ID
                                     (mew-case-folder case mbox)
                                     (vector
                                      ;;"U"
                                      (if (mew-mbox-imap-alist-is-updating mbox plist) "*" "")
                                      ;;"P""
                                      "%"
                                      ;; "LAST-UPDATE"
                                      (if timestamp
                                          (format-time-string "%Y%m%d %H:%M:%S" timestamp)
                                        "???")
                                      ;; "MAIL-TOTAL"
                                      (format "%d" messages)
                                      ;; "MAIL-NEW"
                                      (format "%d" (mew-mbox-imap-alist-mail-new msgid messages uidnext))
                                      ;; "CASE"
                                      (if case case "default")
                                      ;; MBOX
                                      mbox))))))
    entries))


(defun mew-mbox-imap-alist-set(alist mbox key value)
  "设置alist中邮箱mbox的元数据"
  (when alist
    (let ((data (alist-get key alist nil 'remove 'string=)) )
      (if data
          (cond
           ((eq key 'INDEX) (aref data 0 value))
           ((eq key 'TIMESTAMP) (aref data 1 value))
           ((eq key 'MSGID) (aset data 2 value))
           ((eq key 'MESSAGES) (aset data 3 value))
           ((eq key 'RECENT) (aset data 4 value))
           ((eq key 'UIDNEXT) (aset data 5 value))
           ((eq key 'UIDVALIDITY) (aset data 6 value))
           ((eq key 'UNSEEN) (aset data 7 value))
           (t (mew-mbox-msg "set: unknown key %s" key)))
        (mew-mbox-msg "set: mbox %s not found" mbox)
        ))))

(defun mew-mbox-imap-alist-get(alist mbox key)
  "读取alist中邮箱mbox的元数据"
  (if alist
      (let ((data  (alist-get key alist nil 'remove 'string=)) )
        (if data
            (cond
             ((eq key 'INDEX) (aref data 0))
             ((eq key 'TIMESTAMP) (aref data 1))
             ((eq key 'MSGID) (aref data 2))
             ((eq key 'MESSAGES) (aref data 3))
             ((eq key 'RECENT) (aref data 4))
             ((eq key 'UIDNEXT) (aref data 5))
             ((eq key 'UIDVALIDITY) (aref data 6))
             ((eq key 'UNSEEN) (aref data 7))
             (t (progn (mew-mbox-msg "get: unknown key %s" key) nil)))
          (progn 
            (mew-mbox-msg "get: mbox %s not found" mbox)
            nil)
          ))
    nil))

(defun mew-mbox-imap-alist-has-mbox (alist mbox)
  "判断mbox是否在alist中"
  (if (alist-get mbox alist nil 'remove 'string=) t nil))

(defun mew-mbox-imap-alist-merge (alist-old alist-new)
  "合并两个alist数据，以alist-new为骨架，\
将alist-old中的元数据复制到alist-new中，如果\
alist-old的mbox条目在alist-new中没有，则丢弃"
  (dolist (el alist-new)
    (if (mew-mbox-imap-has-mbox alist-old (car el))
        (let ((timestamp (mew-mbox-imap-alist-get alist-old (car el) 'TIMESTAMP))
              (msgid (mew-mbox-imap-alist-get alist-old (car el) 'MSGID))
              (messages (mew-mbox-imap-alist-get alist-old (car el) 'MESSAGES))
              (recent (mew-mbox-imap-alist-get alist-old (car el) 'RECENT))
              (uidnext (mew-mbox-imap-alist-get alist-old (car el) 'UIDNEXT))
              (uidvalidity (mew-mbox-imap-alist-get alist-old (car el) 'UIDVALIDITY))
              (unseen (mew-mbox-imap-alist-get alist-old (car el) 'UNSEEN)))
              (mew-mbox-imap-alist-set alist-new (car el) 'TIMESTAMP timestamp)
              (mew-mbox-imap-alist-set alist-new (car el) 'MSGID msgid)
              (mew-mbox-imap-alist-set alist-new (car el) 'MESSAGES messages)
              (mew-mbox-imap-alist-set alist-new (car el) 'RECENT recent)
              (mew-mbox-imap-alist-set alist-new (car el) 'UIDNEXT uidnext)
              (mew-mbox-imap-alist-set alist-new (car el) 'UIDVALIDITY uidvalidity)
              (mew-mbox-imap-alist-set alist-new (car el) 'UNSEEN unseen))
      (mew-mbox-msg "merge: drop %s" (car el))))
  alist-new)

(defun mew-mbox-imap-update-func (buffer)
  "元数据定时更新函数，触发一个更新进程组"
  (mew-mbox-start-upgrade-process-group buffer))

(defun mew-mbox-count-upgrade-process (process-alist type)
  "查询特性类型的进程数，类型: foreground, background"
  (let ((process-cnt 0) (process-slot) (process-type))
    (dolist (process-slot process-alist)
      (setq process-type (plist-get (cdr process-slot) 'type))
      (when (eq process-type type) (1+ process-cnt)))
    process-cnt))

(defun mew-mbox-next-updatable-mbox (process-alist mbox-alist)
  "返回下一个应该更新的mbox, 如果全部更新过，返回nil"
  (when mbox-alist
    (let ((max-mbox-index 0) (mbox) (mbox-index) (process-slot))
      (dolist (process-slot process-alist )
        (setq mbox (car process-slot))
        (setq mbox-index (mew-mbox-imap-alist-get mbox-alist mbox 'INDEX))
        (when (and mbox-index  (> mbox-index max-mbox-index))
          (setq max-mbox-index mbox-index)))
      (cond
       ((equal (1- (length mbox-alist))  max-mbox-index) nil)
       (t (car (nth (1+ max-mbox-index) mbox-alist)))))))

(defun mew-mbox-imap-kill-update-process-group (buffer)
  "杀死更新进程（组）"
  (let ((mutex (mew-mbox-buffer-get-property buffer 'mutex)))
    (with-mutex mutex
      (let ((process-alist (mew-mbox-buffer-get-property buffer 'process-alist)))
        (dolist (process-slot process-alist)
          ;; 杀死进程，进程的sentinel会清除process-alist
          (let ((process (plist-get (cdr process-slot) 'process)))
            (when process
              (kill-process process))))))))

(defun mew-mbox-start-upgrade-process-group (buffer)
  "启动一个后台更新进程组"
  (let ( (mutex (mew-mbox-buffer-get-property buffer 'mutex)))
    (with-mutex mutex
      (let ((process-alist (mew-mbox-buffer-get-property buffer 'process-alist))
            (mbox-alist (mew-mbox-buffer-get-property buffer 'mbox-alist)))
        ;; 如果没有后台更新进程在运行, 就启动一些后台更新进程
        (unless (equal 0 (mew-mbox-count-upgrade-process process-alist 'background))
          (let ((mbox) (process-slot))
            (dotimes (i mew-mbox-imap-checker-max-background-count)
              (setq mbox (mew-mbox-next-updatable-mbox))
              (setq process-slot (mew-mbox-start-upgrade-process-inter mbox 'background))
              (push process-slot process-alist)
              (mew-mbox-buffer-set-property buffer 'process-alist process-alist))))))))

(defun mew-mbox-start-upgrade-process (buffer mbox)
  "启动一个前台更新进程，前台进程不被mew-mbox-imap-checker-max-background-count约束"
  (let ( (mutex (mew-mbox-buffer-get-property buffer 'mutex)))
    (with-mutex mutex
      (let ((process-alist (mew-mbox-buffer-get-property buffer 'process-alist))
            (mbox-alist (mew-mbox-buffer-get-property buffer 'mbox-alist)))
        (if (not (assoc mbox process-alist)) ;; 如果mbox在更新列表里，就不启动
            (progn
              (setq process-slot (mew-mbox-start-upgrade-process-inter mbox 'foreground))
              (push process-slot process-alist)
              (mew-mbox-buffer-set-property buffer 'process-alist process-alist))
          (message "%s already being updated, please try later"))))))

(defun mew-mbox-start-upgrade-process-inter (mbox type)
  "启动一个更新进程，返回一个进程元数据条目，对元数据不加锁。返回一个进程slot"
  
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 我们会向buffer附加一系列元数据
;; update-timer: 定时器，定时启动一个批量更新的进程组
;; mbox-alist: mbox的元数据
;; process-alist: 进程组，每一个元素是(mbox 'process 'type)
;; mutex: 保护mbox-alist和process-alist的mutex，我们只用一个mutex，避免死锁
(defun mew-mbox-imap-init (buffer)
  "初始化函数，为buffer添加mbox的alist，以及更新alist的定时器update-timer"
  (mew-mbox-msg "mew-mbox-imap-init called %s" buffer)
  (let ((case (mew-mbox-buffer-get-property buffer 'case))
        (proto (mew-mbox-buffer-get-property buffer 'proto)))
    (mew-mbox-buffer-set-property buffer 'mbox-alist (mew-mbox-imap-alist-load case))
    (mew-mbox-buffer-set-property buffer
                                  'mutex (make-mutex (format "mutex-%s:%s" case proto)))
    (mew-mbox-buffer-set-property buffer
                                  'process-alist nil)
    (mew-mbox-buffer-set-property buffer
                                  'update-timer
                                  (run-with-idle-timer
                                   mew-mbox-imap-update-interval t 'mew-mbox-imap-update-func buffer))
    ))

;; 删除timer，杀死所有checker进程
(defun mew-mbox-imap-quit (buffer)
  "杀死timer和所有更新线程，进程，保存元数据"
  (mew-mbox-msg "mew-mbox-imap-quit called %s buffer")
  (let* ((case (mew-mbox-buffer-get-property buffer 'case))
         (alist (mew-mbox-buffer-get-property buffer 'mbox-alist))
         (timer (mew-mbox-buffer-get-property buffer 'update-timer)))
    (cancel-timer timer)
    (mew-mbox-imap-kill-update-process-group buffer)
    (mew-mbox-imap-alist-save case alist)))

(defun mew-mbox-imap-update-mbox-count (buffer mbox)
  "启动一个更新进程，更新特定mbox条目的元数据"
  (mew-mbox-msg "mew-mbox-imap-update-mbox-count called with (%s %s)" buffer mbox)
  (mew-mbox-start-upgrade-process buffer mbox))

(defun mew-mbox-imap-update-all (buffer)
  "启动一个更新进程组，更新所有元数据"
  (mew-mbox-msg "mew-mbox-imap-update-all called")
  (mew-mbox-start-upgrade-process-group buffer))

(defun mew-mbox-imap-generate-entries (buffer no-zero-na)
  "从元数据中产生entries并返回"
  (mew-mbox-msg "mew-mbox-imap-generate-entries called with (%s %s)" buffer no-zero-na)
  (let ((entries)
        (mutex (mew-mbox-buffer-get-property buffer 'mutex))
        (with-mutex mutex
          (setq entries (mew-mbox-imap-alist-export
                         (mew-mbox-buffer-get-property buffer 'case)
                         (mew-mbox-buffer-get-property buffer 'mbox-alist)
                         (mew-mbox-buffer-get-property buffer 'process-alist))))
        entries)))


;; 启动mew时会调用：mew-init-p ＝nil
;; Z 时会调用：mew-init-p ＝t  
(defun mew-mbox-imap-status-update (buffer init)
  "Mew启动后会调用该函数，init为nil时，什么都不用做; init为t时\
表示Mew重新从服务器读取了邮箱列表，我们需要更新元数据"
  (mew-mbox-msg "mew-mbox-imap-status-update called with (%s)" init)
  (when (and init buffer)
    (let* ((case (mew-mbox-buffer-get-property buffer 'case))
           (new-alist (mew-mbox-imap-alist-load case t))
           (old-alist (mew-mbox-buffer-get-property buffer 'mbox-alist))
           (mutex (mew-mbox-buffer-get-property buffer 'mutex)))
      (with-mutex mutex
        (setq new-alist
              (mew-mbox-imap-alist-merge old-alist new-alist))
        (mew-mbox-buffer-set-property buffer 'mbox-alist new-alist)
        (mew-mbox-imap-alist-save case new-alist)))))

;; 注册协议
(mew-mbox-register-proto 'imap
                         'mew-mbox-imap-init
                         'mew-mbox-imap-quit
                         'mew-mbox-imap-status-update
                         'mew-mbox-imap-update-mbox
                         'mew-mbox-imap-update-all
                         'mew-mbox-imap-generate-entries)


(provide 'mew-mbox-imap)
