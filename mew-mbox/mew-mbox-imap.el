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

(defcustom mew-mbox-imap-checker-max-background-count 3
  "最多同时运行的异步后台imap checker数"
  :group 'mew-env
  :type 'integer)

(defcustom mew-mbox-imap-update-interval 300
  "两轮imap checker扫描的时间间隔（秒）"
  :group 'mew-env
  :type 'integer)

(defcustom mew-mbox-imap-delay-interval 10
  "两个邮箱扫描的等待时间间隔（秒），防止把邮箱打死"
  :group 'mew-env
  :type 'integer)

(defcustom mew-mbox-imap-alist-save-num 10
  "扫描多少轮邮箱就保存一次元数据"
  :group 'mew-env
  :type 'integer)


(defcustom mew-mbox-imap-exclude-list nil
  "有哪些邮箱需要被排除在扫描列表里"
  :group 'mew-env)

(defvar mew-mbox-imap-alist-save-cnt 0 "一共进行了多少轮的邮箱扫描")

(defconst mew-mbox-imap-cache-file ".mew-mbox-cache-imap")
(defconst mew-mbox-imap-checker-prefix "mew-mbox-imap-checker-")

;;;;;;;;;;;;;;;;;;;;;mbox-alist元数据相关函数
(defun mew-mbox-imap-alist-load (case force-rebuild &optional exclude-list)
  "装载mbox元数据，返回alist 或者 nil，优先读取cache文件，\
如果没有cache文件，直接重新构建一个。\
如果force-rebuild为t，强制重新构建 \
元数据格式：
  ((mbox-name . [INDEX TIMESTAMP MSGID L-MESSAGES R-MESSAGES RECENT UIDNEXT UIDVALIDITY UNSEEN]) ... )
  INDEX: 条目在整个alist中的index
  TIMESTAMP: 更新时间戳
  MSGID: 从邮箱的msg-id文件中读取的数值
  L-MESSAGES: 本地邮箱的邮件数
  R-MESSAGES: 远程邮箱的邮件数（IMAP 返回的MESSAGES）
  RECENT: IMAP返回的RECENT
  UIDNEXT: IMAP返回的UIDNEXT
  UIDVALIDITY: IMAP返回的UIDVALIDITY
  UNSEEN: IMAP返回的UNSEEN"
  (mew-mbox-log-info "mew-mbox-imap-alist-load %s force %s" case force-rebuild)
  (let* ((alist) (mbox-list) (index 0)
         (mbox-path (mew-expand-folder (mew-case-folder case "%")))
         (mbox-alist-file-path (file-name-concat mbox-path mew-mbox-imap-cache-file)))
    (condition-case err 
        (if (or force-rebuild (not (file-exists-p  mbox-alist-file-path)))
            (progn 
              (setq mbox-list (mew-imap-folder-alist case))
              (dolist (mbox-list-slot mbox-list)
                (let ((mbox (car mbox-list-slot)))
                  (when (mew-mbox-imap-filter-mbox mbox exclude-list)
                    (setq alist (append alist (list (cons mbox
                                                          (vector index nil nil nil nil nil nil nil nil)))))
                    (setq index (1+ index))))))
          (progn
            (setq alist (mew-lisp-load mbox-alist-file-path))))
      (error (message "mew-mbox-imap-alist-load failed %s" (error-message-string err))))
    alist))

(defun mew-mbox-imap-alist-save (case alist)
  "保存mbox元数据"
  (mew-mbox-log-info "mew-mbox-imap-alist-save %s" case)
  (let* ((mbox-path (mew-expand-folder (mew-case-folder case "%")))
         (mbox-alist-file-path (file-name-concat mbox-path mew-mbox-imap-cache-file)))
    (condition-case err
        (mew-lisp-save mbox-alist-file-path alist t t)
      (error (message "mew-mbox-imap-alist-save failed %s" (error-message-string err))))))

(defun mew-mbox-imap-alist-mail-new (msgid messages uidnext)
  "计算有多少邮件是未读的"
  (let ((ret 0))
    (setq msgid (or msgid 0))
    (setq messages (or messages 0))
    (setq uidnext (or uidnext 0))    
    (if (= 0 messages)
        (setq ret 0) ;; 显示为0
      (progn
        (if (equal 0 msgid) ;; 如果msgid == 0
            (setq ret messages) ;; 显示messages
          (setq ret (- (- uidnext msgid) 1))) ;; 否则计算uidnext和msgid的差距
        ))
    ret))

(defun mew-mbox-imap-alist-is-updating (mbox plist)
  "mbox 是否正在更新？本质是检查mbox是否在plist里"
  (if (assoc mbox plist) t nil))

(defun mew-mbox-imap-filter-mbox (mbox filter)
  "匹配mbox和filter中每一个元素，如果匹配就返回nil，否则返回t"
  (not (reduce (lambda (&optional x y) (or x y)) (mapcar  (lambda (x) (string-match x mbox)) filter ))))

(defun mew-mbox-imap-alist-export(buffer case alist plist name-filter new-filter)
  "export mbox的元数据到list的entry数据"
  (let ((entries) (alist-slot) (biff-cnt 0))
    (dolist (alist-slot alist)
      (let* ((mbox (car alist-slot))
             (timestamp (mew-mbox-imap-alist-get alist mbox 'TIMESTAMP))
             (msgid (mew-mbox-imap-alist-get alist mbox 'MSGID))
             (l-messages (mew-mbox-imap-alist-get alist mbox 'L-MESSAGES))
             (r-messages (mew-mbox-imap-alist-get alist mbox 'R-MESSAGES))
             (unseen (mew-mbox-imap-alist-get alist mbox 'UNSEEN))
             (uidnext (mew-mbox-imap-alist-get alist mbox 'UIDNEXT))
             (new-mail (mew-mbox-imap-alist-mail-new msgid r-messages uidnext)))
        ;; 累加biff-cnt
        (setq biff-cnt (+ biff-cnt new-mail))
        (when (and (mew-mbox-imap-filter-mbox mbox name-filter) (or (not (equal new-mail 0)) (and new-filter (equal 0 new-mail))))
          (setq entries (append entries
                                (list
                                 (list
                                  ;; ID
                                  (list case mbox)
                                  (vector
                                   ;;"U"
                                   (if (mew-mbox-imap-alist-is-updating mbox plist) "*" "")
                                   ;; MBOX
                                   (if (= new-mail 0)
                                       (if (= 0 unseen)
                                           mbox
                                         (propertize mbox 'face 'mew-mbox-unseen-mail-face))
                                     (propertize mbox 'face 'mew-mbox-new-mail-face))
                                   ;; "LAST-UPDATE"
                                   (if timestamp
                                       (format-time-string "%Y/%m/%d %H:%M:%S" timestamp)
                                     "???")
                                   ;; "MAIL-NEW"
                                   (format "%d" new-mail)
                                   ;; "MAIL-REMOTE"
                                   (format "%d" (or r-messages 0))
                                   ;; "MAIL-LOCAL"
                                   (format "%d" (or l-messages 0))
                                   ;; "UNSEEN"
                                   (format "%d" (or unseen 0))
                                   ;; UIDNEXT
                                   (format "%d" (or uidnext 0))
                                   ;; MSG-ID
                                   (format "%d" (or msgid 0))
                                   ))))))))
    (mew-mbox-buffer-set-property buffer 'biff-cnt biff-cnt)
    entries))


(defun mew-mbox-imap-alist-set(alist mbox key value)
  "设置alist中邮箱mbox的元数据"
  (when alist
    (let ((data (alist-get mbox alist nil 'remove 'string=)) )
      (if data
          (cond
           ((eq key 'INDEX) (aset data 0 value))
           ((eq key 'TIMESTAMP) (aset data 1 value))
           ((eq key 'MSGID) (aset data 2 value))
           ((eq key 'L-MESSAGES) (aset data 3 value))
           ((eq key 'R-MESSAGES) (aset data 4 value))
           ((eq key 'RECENT) (aset data 5 value))
           ((eq key 'UIDNEXT) (aset data 6 value))
           ((eq key 'UIDVALIDITY) (aset data 7 value))
           ((eq key 'UNSEEN) (aset data 8 value))
           (t (mew-mbox-log-warn "set: unknown key %s" key)))
        (mew-mbox-log-warn "set: mbox %s not found" mbox)
        ))))

(defun mew-mbox-imap-alist-get(alist mbox key)
  "读取alist中邮箱mbox的元数据"
  (if alist
      (let ((data  (alist-get mbox alist nil 'remove 'string=)) )
        (if data
            (cond
             ((eq key 'INDEX) (aref data 0))
             ((eq key 'TIMESTAMP) (aref data 1))
             ((eq key 'MSGID) (aref data 2))
             ((eq key 'L-MESSAGES) (aref data 3))
             ((eq key 'R-MESSAGES) (aref data 4))
             ((eq key 'RECENT) (aref data 5))
             ((eq key 'UIDNEXT) (aref data 6))
             ((eq key 'UIDVALIDITY) (aref data 7))
             ((eq key 'UNSEEN) (aref data 8))
             (t (progn (mew-mbox-log-warn "get: unknown key %s" key) nil)))
          (progn 
            (mew-mbox-log-warn "get: mbox %s not found" mbox)
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
  (dolist (el alist-old)
    (if (mew-mbox-imap-alist-has-mbox alist-new (car el))
        (let ((timestamp (mew-mbox-imap-alist-get alist-old (car el) 'TIMESTAMP))
              (msgid (mew-mbox-imap-alist-get alist-old (car el) 'MSGID))
              (l-messages (mew-mbox-imap-alist-get alist-old (car el) 'L-MESSAGES))
              (r-messages (mew-mbox-imap-alist-get alist-old (car el) 'R-MESSAGES))
              (recent (mew-mbox-imap-alist-get alist-old (car el) 'RECENT))
              (uidnext (mew-mbox-imap-alist-get alist-old (car el) 'UIDNEXT))
              (uidvalidity (mew-mbox-imap-alist-get alist-old (car el) 'UIDVALIDITY))
              (unseen (mew-mbox-imap-alist-get alist-old (car el) 'UNSEEN)))
          (mew-mbox-imap-alist-set alist-new (car el) 'TIMESTAMP timestamp)
          (mew-mbox-imap-alist-set alist-new (car el) 'MSGID msgid)
          (mew-mbox-imap-alist-set alist-new (car el) 'L-MESSAGES l-messages)
          (mew-mbox-imap-alist-set alist-new (car el) 'R-MESSAGES r-messages)
          (mew-mbox-imap-alist-set alist-new (car el) 'RECENT recent)
          (mew-mbox-imap-alist-set alist-new (car el) 'UIDNEXT uidnext)
          (mew-mbox-imap-alist-set alist-new (car el) 'UIDVALIDITY uidvalidity)
          (mew-mbox-imap-alist-set alist-new (car el) 'UNSEEN unseen))
      (mew-mbox-log-info "merge: drop mbox %s" (car el))))
  alist-new)

(defun mew-mbox-imap-alist-update(alist mbox val)
  "从val中更新alist对应mbox条目的元数据"
  (let ((alist-new (list (cons mbox val))))
    (mew-mbox-imap-alist-merge alist-new alist)))

;;;;;;;;;;;;;;;;;;;;;bg-todo-mbox-list元数据相关函数
(defun mew-mbox-imap-todo-has-pending (buffer)
  "是否有没有完成的后台更新作业列表？"
  (if (mew-mbox-buffer-get-property buffer 'bg-todo-mbox-list) t nil))

(defun mew-mbox-imap-todo-next (buffer)
  "从后台更新列表中返回下一个应该更新的项目, 并从todo列表里删除"
  (let ((bg-todo-mbox-list (mew-mbox-buffer-get-property buffer 'bg-todo-mbox-list))
        (ret))
    (when bg-todo-mbox-list
      (setq ret (car bg-todo-mbox-list))
      (setq bg-todo-mbox-list (cdr bg-todo-mbox-list))
      (mew-mbox-buffer-set-property buffer 'bg-todo-mbox-list bg-todo-mbox-list))
    ret))

(defun mew-mbox-imap-todo-new (buffer)
  "创建一个后台更新列表，原来的不要了"
  (let ((bg-todo-mbox-list)
        (mbox-alist (mew-mbox-buffer-get-property buffer 'mbox-alist)))
    (dolist (elm mbox-alist)
      (setq bg-todo-mbox-list (append bg-todo-mbox-list (list (car elm)))))
    (mew-mbox-buffer-set-property buffer 'bg-todo-mbox-list bg-todo-mbox-list)))


(defun mew-mbox-imap-todo-clear (buffer)
  "清除后台更新列表"
  (mew-mbox-buffer-set-property buffer 'bg-todo-mbox-list nil))

;;;;;;;;;;;;;;;;;;;;;process-alist元数据相关函数
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
  (mew-mbox-log-debug "mew-mbox-start-upgrade-process-group called")
  (let ( (mutex (mew-mbox-buffer-get-property buffer 'mutex)) (ret))
    (with-mutex mutex
      (let* ((process-alist (mew-mbox-buffer-get-property buffer 'process-alist))
             (process-cnt (mew-mbox-count-upgrade-process process-alist 'background)))
        ;; 就启动一些后台更新进程，如果process-cnt < max
        (when (< process-cnt mew-mbox-imap-checker-max-background-count)
          (unless (mew-mbox-imap-todo-has-pending buffer)
            (mew-mbox-imap-todo-new buffer))
          (let ((mbox)
                (process-slot)
                (case (mew-mbox-buffer-get-property buffer 'case))
                (alist (mew-mbox-buffer-get-property buffer 'mbox-alist)))
            (dotimes (i (- mew-mbox-imap-checker-max-background-count process-cnt))
              (setq mbox (mew-mbox-imap-todo-next buffer))
              (when mbox
                (setq process-slot (mew-mbox-start-upgrade-process-inter buffer mbox 'background))
                (when process-slot
                  (push process-slot process-alist)
                  (mew-mbox-buffer-set-property buffer 'process-alist process-alist)
                  (setq mew-mbox-imap-alist-save-cnt (1+ mew-mbox-imap-alist-save-cnt))
                  (when (= (% mew-mbox-imap-alist-save-cnt mew-mbox-imap-alist-save-num) 0)
                    (mew-mbox-imap-alist-save case alist))
                  (setq ret t))))))))
    ret))

(defun mew-mbox-start-upgrade-process (buffer mbox)
  "启动一个前台更新进程，前台进程不被mew-mbox-imap-checker-max-background-count约束"
  (let ( (mutex (mew-mbox-buffer-get-property buffer 'mutex)) (ret))
    (with-mutex mutex
      (let ((process-alist (mew-mbox-buffer-get-property buffer 'process-alist))
            (mbox-alist (mew-mbox-buffer-get-property buffer 'mbox-alist)))
        (if (not (assoc mbox process-alist)) ;; 如果mbox在更新列表里，就不启动
            (when (assoc mbox mbox-alist) ;; 如果mbox已经被排除了，就不启动
              (setq process-slot (mew-mbox-start-upgrade-process-inter buffer mbox 'foreground))
              (when process-slot            
                (push process-slot process-alist)
                (mew-mbox-buffer-set-property buffer 'process-alist process-alist)
                (setq ret t)))
          (message "%s already being updated, please try later" mbox))))
    ret))


(defun mew-mbox-imap-checker-get (proc key)
  "设置进程属性值"
  (let ((r-key (concat mew-mbox-imap-checker-prefix key)))
    (process-get proc (intern r-key))))

(defun mew-mbox-imap-checker-set (proc key val)
  "获得进程属性值"
  (let ((r-key (concat mew-mbox-imap-checker-prefix key)))
    (process-put proc (intern r-key) val)))


(defun mew-mbox-count-upgrade-process (process-alist type)
  "查询特性类型的进程数，类型: foreground, background"
  (let ((process-cnt 0))
    (dolist (process-slot process-alist)
      (when (eq (mew-mbox-imap-checker-get (cadr process-slot) "type") type)
        (setq process-cnt (1+ process-cnt))))
    process-cnt))

(defun mew-mbox-imap-checker-sentinel (proc event)
  "守护进程，在check退出的时候调用"
  (let*  ((buffer (mew-mbox-imap-checker-get proc "buffer"))
          (mutex (mew-mbox-buffer-get-property buffer 'mutex))
          (case (mew-mbox-buffer-get-property buffer 'case))
          (mbox (mew-mbox-imap-checker-get proc "mbox"))
          (output (mew-mbox-imap-checker-get proc "output"))
          (type (mew-mbox-imap-checker-get proc "type"))
          (proc-status (process-status proc))
          (proc-exit-code (process-exit-status proc)))
    (with-mutex mutex
      (let ((alist (mew-mbox-buffer-get-property buffer 'mbox-alist))
            (plist (mew-mbox-buffer-get-property buffer 'process-alist))
            (mbox-next) (process-slot))
        (mew-mbox-log-debug "[sentinel %s] '%s:%s' output '%s'"
                            proc case mbox output)
        (cond
         ((or (eq proc-status 'run) (eq proc-status 'stop))
          ((mew-mbox-log-info "[checker %s:%s %s] still running or stopped, kill it" case mbox type))
          (kill-process proc))
         ((eq proc-status 'signal)
          (mew-mbox-log-info "[checker %s:%s %s] killed with %d" case mbox type proc-exit-code))
         ((eq proc-status 'exit)
          (mew-mbox-log-info "[checker %s:%s %s] exited with %d" case mbox type  proc-exit-code)
          (if (= 0 proc-exit-code)
              (let ((val (mew-mbox-imap-checker-parse case mbox output)))
                (if val (mew-mbox-imap-alist-update alist mbox val)
                  (mew-mbox-log-warn "[checker %s:%s %s] output parse error" case mbox type output)))
            (mew-mbox-log-warn "[checker %s:%s %s] error output %s" case mbox type output))))
        (setq plist (assoc-delete-all mbox plist 'string=))
        (mew-mbox-log-debug "plist cnt %d" (mew-mbox-count-upgrade-process plist 'background))
        (when (eq 'background type) ;; 只有后台进程结束后，才启动新的
          (setq mbox-next (mew-mbox-imap-todo-next buffer))
          (when mbox-next ;; 还有mbox可以更新，启动下一个更新进程
            (setq process-slot (mew-mbox-start-upgrade-process-inter buffer mbox-next 'background))
            (when process-slot
              (push process-slot plist))))     
        (mew-mbox-buffer-set-property buffer 'process-alist plist)))))

(defun mew-mbox-imap-checker-filter (proc string)
  (let ((output  (mew-mbox-imap-checker-get proc "output")))
    (setq output (concat output string))
    (mew-mbox-imap-checker-set proc "output" output)))

(defun mew-mbox-start-upgrade-process-inter (buffer mbox type)
  "启动一个更新进程，返回一个进程元数据条目，对元数据不加锁。返回一个进程slot"
  (let* ((case (mew-mbox-buffer-get-property buffer 'case))
         (proc-path mew-mbox-imap-checker-proc)
         (server (mew-imap-server case))
         (port (mew-imap-port case))
         (sslp (mew-imap-ssl case))
         (sslport (mew-imap-ssl-port case))
         (user (mew-imap-user case))
         (passwd (mew-mbox-imap-get-pass buffer type))
         (delay (format "%d" ;; 如果是前台进程，不要延迟
                        (if (eq type 'background)
                            mew-mbox-imap-delay-interval
                          0)))
         (mailbox (mew-imap-utf-7-encode-string
                   (mew-imap-bnm-to-mailbox mbox)))
         (proc-name (format "%s%s%s" mew-mbox-imap-checker-prefix case mbox))
         (process))
    (if (numberp port)
        (setq port (number-to-string port))
      (setq port "143"))
    (if (numberp sslport)
        (setq sslport (number-to-string sslport))
      (setq sslport "993"))
    (if (and passwd (file-executable-p proc-path))
        (progn
          (setq process (condition-case err
                            (start-process proc-name nil proc-path
                                           "-s" server
                                           "-p" (if sslp sslport port)
                                           "-u" user
                                           "-m" mailbox
                                           "-S" (if sslp "yes" "no")
                                           "-n" delay)
                          (error (progn
                                   (mew-mbox-log-warn "can not start imap checker: %s"
                                                      (error-message-string err)) nil))))
          (when process
            (set-process-sentinel process 'mew-mbox-imap-checker-sentinel)
            (set-process-filter process 'mew-mbox-imap-checker-filter)
            (mew-mbox-imap-checker-set process "buffer" buffer)
            (mew-mbox-imap-checker-set process "mbox" mbox)
            (mew-mbox-imap-checker-set process "type" type)
            (process-send-string process (format "%s\n" passwd))))
      (if (not passwd)
          (mew-mox-warn "mew-mbox: password is null, can not start process")
        (mew-mbox-log-warn "mew-mbox: %s is not excutable or exist, can not start process" proc-path)))
    (if process
        (progn
          (mew-mbox-log-info "[checker %s:%s %s started]" case mbox type)
          (list mbox process))
      nil)))

;;;;;;;;;;;;;;;;;;;;;timger定时函数
(defun mew-mbox-imap-update-func (buffer)
  "元数据定时更新函数，触发一个更新进程组"
  (mew-mbox-buffer-set-property buffer
                                'update-timer
                                (run-with-timer
                                 mew-mbox-imap-update-interval nil 'mew-mbox-imap-update-func buffer))
  (mew-mbox-start-upgrade-process-group buffer))


;;;;;;;;;;;;;;;;;;;;;mbox元数据解析
(defun mew-mbox-imap-load-msgid (case mbox)
  "获取 msg-id "
  (let ((msgid-file (mew-expand-file (mew-case-folder case mbox) mew-imap-msgid-file))
        (msgid 0) (msgid-str))
    (condition-case err
        (progn 
          (setq msgid-str (or (mew-lisp-load msgid-file) "0"))
          (setq msgid (string-to-number msgid-str)))
      (error
       (mew-mbox-log-warn "load msgid file %s for %s %s error %s"
                          msgid-file case mbox (error-message-string err))))
    msgid))

(defun mew-mbox-imap-get-message-count (case mbox)
  "获取邮箱中的消息数"
  (let ((mbox-path (mew-expand-folder (mew-case-folder case mbox)))
        (msg-count 0) (file-list))
    (condition-case err
        (progn 
          (setq file-list (directory-files-and-attributes
                           (mew-expand-folder (mew-case-folder case mbox))  nil "^[0-9]+"))
          (dolist (elm file-list)
            (unless (car (cdr elm))
              (setq msg-count (1+ msg-count)))))
      (error
       (mew-mbox-log-warn "Load msg count dir %s for %s %s error %s"
                          mbox-path case mbox (error-message-string err))))
    msg-count))

;; (MESSAGES RECENT UIDNEXT UIDVALIDITY UNSEEN) ->
;; [INDEX TIMESTAMP MSGID L-MESSAGES R-MESSAGES RECENT UIDNEXT UIDVALIDITY UNSEEN]
(defun mew-mbox-imap-checker-parse (case mbox output)
  "解析 checker 的输出，结合其他信息，构造可以更新元数据的val数组"
  (when output
    (let* ((val (split-string output "[ ]" t))
           (timestamp (current-time))
           (msg-id (mew-mbox-imap-load-msgid case mbox))
           (l-messages (mew-mbox-imap-get-message-count case mbox))
           (r-messages (string-to-number (nth-value 0 val)))
           (recent (string-to-number (nth-value 1 val)))
           (uidnext (string-to-number (nth-value 2 val)))
           (uidvalidity (string-to-number (nth-value 4 val)))
           (unseen (string-to-number (nth-value 4 val))))
      (while (not timestamp) (setq (timestamp (current-time)))) ;; current-time 有时返回nil?
      (vector 0 timestamp msg-id l-messages r-messages recent uidnext uidvalidity unseen))))

;;;;;;;;;;;;;;;;;;;;;其他函数
(defun mew-mbox-imap-get-pass (buffer type)
  "获取mew的缓存密码，或者是提示用户输入密码，取决于type:
  type是'background: 如果有缓存返回缓存，没有返回nil
  type是'foreground: 如果有缓存返回返回，没有提示用户输入"
  (let* ((case (mew-mbox-buffer-get-property buffer 'case))
         (server (mew-imap-server case))
         (user (mew-imap-user case))
         (prompt (format "IMAP password (%s@%s): " user server)))
    (if (eq 'foreground type);; 只有前台进程才让用户输入密码，后台的没有缓存密码就失败了
        (mew-input-passwd prompt (mew-imap-passtag2 nil))
      (mew-passwd-get-passwd (mew-imap-passtag2 case)) )))

;;; 添加advice监测如下函数
;;; (mew-imap-retrieve case directive bnm range get-body)

(defun before-mew-imap-sentinel (process event)
  "在mew-imap-retrieve之后运行，插入一个更新请求"
  (let* ((pnm (process-name process))
         (directive (mew-imap-get-directive pnm))
         (case (or (mew-imap-get-case pnm) "default"))
         (bnm (mew-imap-get-bnm pnm))
         (error (mew-imap-get-error pnm)))
    (mew-mbox-log-debug "before-mew-imap-sentinel: %s:%s %s %s" case bnm directive error)
    ;;    ))
    (unless error
      (when (and bnm (or (eq directive 'sync) (eq directive 'scan)))
        (let* ((proto (mew-mbox-fld-proto bnm))
               (buffer (mew-mbox-buffer-find case proto)))
          (when buffer
            (mew-mbox-start-upgrade-process buffer bnm)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 以下是对外接口函数

;; 我们会向buffer附加一系列元数据
(defun mew-mbox-imap-init (buffer)
  "初始化函数，为buffer添加一系列元数据:
   update-timer: 定时器，定时启动一个批量更新的后台进程组
   mbox-alist: mbox的元数据
   process-alist: 进程组，每一个元素是(mbox 'process 'type)，包含前台\后台进程
   bg-todo-mbox-list: 一个后台进程组的作业列表
   mutex: 保护mbox-alist和process-alist的mutex，我们只用一个mutex，避免死锁"
  (mew-mbox-log-debug "mew-mbox-imap-init called %s" buffer)
  (mew-mbox-imap-get-pass buffer 'foreground)
  (let ((case (mew-mbox-buffer-get-property buffer 'case))
        (proto (mew-mbox-buffer-get-property buffer 'proto)))
    (mew-mbox-buffer-set-property buffer 'mbox-alist (mew-mbox-imap-alist-load case nil))
    (mew-mbox-buffer-set-property buffer
                                  'mutex (make-mutex (format "mutex-%s:%s" case proto)))
    (mew-mbox-buffer-set-property buffer
                                  'process-alist nil)
    (mew-mbox-buffer-set-property buffer
                                  'bg-todo-mbox-list nil)    
    (mew-mbox-buffer-set-property buffer
                                  'update-timer
                                  (run-with-timer
                                   mew-mbox-imap-update-interval nil 'mew-mbox-imap-update-func buffer))
    (advice-add 'mew-imap-sentinel :before 'before-mew-imap-sentinel)
    ))

;; 删除timer，杀死所有checker进程
(defun mew-mbox-imap-quit (buffer)
  "杀死timer和所有更新线程，进程，保存元数据"
  (mew-mbox-log-debug "mew-mbox-imap-quit called %s" buffer)
  (let ((mutex (mew-mbox-buffer-get-property buffer 'mutex)))
    (with-mutex mutex
      (let* ((case (mew-mbox-buffer-get-property buffer 'case))
             (alist (mew-mbox-buffer-get-property buffer 'mbox-alist))
             (timer (mew-mbox-buffer-get-property buffer 'update-timer)))
        (cancel-timer timer)
        (mew-mbox-buffer-set-property buffer 'update-timer nil)
        (mew-mbox-imap-todo-clear buffer)
        (mew-mbox-imap-kill-update-process-group buffer)
        (mew-mbox-imap-alist-save case alist))))
  (advice-remove 'mew-imap-sentinel 'before-mew-imap-sentinel))

(defun mew-mbox-imap-update-mbox (buffer mbox)
  "启动一个更新进程，更新特定mbox条目的元数据"
  (mew-mbox-log-debug "mew-mbox-imap-update-mbox called with (%s %s)" buffer mbox)
  (mew-mbox-imap-get-pass buffer 'foreground)
  (mew-mbox-start-upgrade-process buffer mbox))

(defun mew-mbox-imap-update-all (buffer)
  "启动一个更新进程组，更新所有元数据"
  (mew-mbox-log-debug "mew-mbox-imap-update-all called")
  (mew-mbox-imap-get-pass buffer 'foreground)
  (mew-mbox-start-upgrade-process-group buffer))

(defun mew-mbox-imap-generate-entries (buffer name-filter new-filter)
  "从元数据中产生entries并返回"
  (mew-mbox-log-debug "mew-mbox-imap-generate-entries called with (%s %s)" buffer name-filter new-filter)
  (let ((entries)
        (mutex (mew-mbox-buffer-get-property buffer 'mutex)))
    (with-mutex mutex
      (setq entries (mew-mbox-imap-alist-export
                     buffer
                     (mew-mbox-buffer-get-property buffer 'case)
                     (mew-mbox-buffer-get-property buffer 'mbox-alist)
                     (mew-mbox-buffer-get-property buffer 'process-alist) name-filter new-filter)))
    entries))


;; 启动mew时会调用：mew-init-p ＝nil
;; Z 时会调用：mew-init-p ＝t  
(defun mew-mbox-imap-status-update (buffer init)
  "Mew启动后会调用该函数，init为nil时，什么都不用做; init为t时\
表示Mew重新从服务器读取了邮箱列表，我们需要更新元数据"
  (mew-mbox-log-debug "mew-mbox-imap-status-update called with (%s)" init)
  (when (and init buffer)
    (mew-mbox-log-info "rebuild mbox alist!")
    (let* ((case (mew-mbox-buffer-get-property buffer 'case))
           (new-alist (mew-mbox-imap-alist-load case t mew-mbox-imap-exclude-list))
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
