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

(defcustom mew-mbox-imap-checker-max-count 10
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
;;((mbox-name . [TIMESTAMP MSGID MESSAGES RECENT UIDNEXT UIDVALIDITY UNSEEN]) ... )
(defun mew-mbox-imap-alist-load (case)
  "装载mbox元数据，返回alist 或者 nil"
  nil)

(defun mew-mbox-imap-alist-save (case alist)
  "保存mbox元数据"
  nil)

(defun mew-mbox-imap-alist-export(alist)
  "export mbox的元数据到list的entry数据"
  )

(defun mew-mbox-imap-alist-set(alist mbox key value)
  "设置alist中邮箱mbox的元数据，注意该函数调用前需要加锁"
  (when alist
    (let ((data (alist-get key alist nil 'remove 'string=)) )
      (if data
        (cond
         ((eq key 'TIMESTAMP) (aref data 0 value))
         ((eq key 'MSGID) (aset data 1 value))
         ((eq key 'MESSAGES) (aset data 2 value))
         ((eq key 'RECENT) (aset data 3 value))
         ((eq key 'UIDNEXT) (aset data 4 value))
         ((eq key 'UIDVALIDITY) (aset data 5 value))
         ((eq key 'UNSEEN) (aset data 6 value))
         (t (mew-mbox-msg "set: unknown key %s" key)))
        (mew-mbox-msg "set: mbox %s not found" mbox)
        ))))

(defun mew-mbox-imap-alist-get(alist mbox key)
  "读取alist中邮箱mbox的元数据，注意该函数调用前需要加锁"
  (if alist
    (let ((data  (alist-get key alist nil 'remove 'string=)) )
      (if data
        (cond
         ((eq key 'TIMESTAMP) (aref data 0))
         ((eq key 'MSGID) (aref data 1))
         ((eq key 'MESSAGES) (aref data 2))
         ((eq key 'RECENT) (aref data 3))
         ((eq key 'UIDNEXT) (aref data 4))
         ((eq key 'UIDVALIDITY) (aref data 5))
         ((eq key 'UNSEEN) (aref data 6))
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
      (setf (alist-get (car el) alist-new nil 'remove 'string=)
            (alist-get (car el) alist-old nil 'remove 'string=))
      (mew-mbox-msg "merge: drop %s" (car el))))
  alist-new)

(defun mew-mbox-imap-update-func (buffer)
  "元数据定时更新函数，触发一个更新进程组"
  (mew-mbox-start-upgrade-process-group buffer))

(defun mew-mbox-imap-kill-update-process-group (buffer)
  "杀死更新进程（组）"
  (mew-mbox-msg "mew-mbox-imap-kill-update-process called with %s" buffer)
  )

(defun mew-mbox-start-upgrade-process-group (buffer)
  "启动一个更新进程组"

(defun mew-mbox-start-upgrade-process (buffer mbox)
  "启动一个更新进程"
  )

;; 我们会向buffer附加一系列元数据
;; update-timer: 定时器，定时启动一个批量更新的进程组
;; mbox-alist: mbox的元数据
;; alist-mutex: 保护alist-mutex的mutex
;; process-alist: 更新进程组，每一个元素是(process . mbox-index)
;; process-mutex: 保护更新进程组的mutex
(defun mew-mbox-imap-init (buffer)
  "初始化函数，为buffer添加mbox的alist，以及更新alist的定时器update-timer"
  (mew-mbox-msg "mew-mbox-imap-init called %s" buffer)
  (let ((case (mew-mbox-buffer-get-property buffer 'case))
        (proto (mew-mbox-buffer-get-property buffer 'proto)))
    (mew-mbox-buffer-set-property buffer 'mbox-alist (mew-mbox-imap-alist-load case))
    (mew-mbox-buffer-set-property buffer
                                  'alist-mutex (make-mutex (format "alist-mutex-%s:%s" case proto)))
    (mew-mbox-buffer-set-property buffer
                                  'process-mutex (make-mutex (format "proc-mutex-%s:%s" case proto)))
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
        (mutex (mew-mbox-buffer-get-property buffer 'alist-mutex))
        (alist (mew-mbox-buffer-get-property buffer 'mbox-alist)))
    (with-mutex mutex
      (setq entries (mew-mbox-imap-export-mbox-alist
                     (mew-mbox-buffer-get-property buffer 'mbox-alist))
      ))
    entries)))


;; 启动mew时会调用：mew-init-p ＝nil
;; Z 时会调用：mew-init-p ＝t  
(defun mew-mbox-imap-status-update (buffer init)
  "Mew启动后会调用该函数，init为nil时，什么都不用做; init为t时\
表示Mew重新从服务器读取了邮箱列表，我们需要更新元数据"
  (mew-mbox-msg "mew-mbox-imap-status-update called with (%s)" init)
  (when (and init buffer)
    (let* ((case (mew-mbox-buffer-get-property buffer 'case))
           (new-alist (mew-mbox-imap-alist-load case))
           (old-alist (mew-mbox-buffer-get-property buffer 'mbox-alist))
           (mutex (mew-mbox-buffer-get-property buffer 'alist-mutex)))
      (with-mutex mutex
        (setq new-alist
              (mew-mbox-imap-alist-merge old-alist new-alist))
        (mew-mbox-buffer-set-property buffer 'mbox-alist new-alist)
        (mew-mbox-imap-alist-save case new-alist)))))

(mew-mbox-register-proto 'imap
                         'mew-mbox-imap-init
                         'mew-mbox-imap-quit
                         'mew-mbox-imap-status-update
                         'mew-mbox-imap-update-mbox
                         'mew-mbox-imap-update-all
                         'mew-mbox-imap-generate-entries)


;; 重新构建mailbox列表
;; (defun mew-mail-sum-imap-rebuild-mbox-alist ()
;;   (mew-mail-sum-msg "mew-mail-sum-imap-rebuild-mbox-alist called")
;;   (let (case alist mbox-alist ret-alist) ;; 重新构建列表
;;     (dolist (case mew-config-cases)
;;       (if (eq (mew-mailbox-type case) 'imap)
;;           (progn
;;             (setq alist (mew-imap-folder-alist case))
;;             (setq mbox-alist nil)
;;             (dolist (mbox alist)
;;               (let* ((mbox-name (car mbox))
;;                      (time-stamp (cons "time-stamp" nil))
;;                      (data (cons "data" (list (mew-mail-sum-imap-load-msgid case mbox-name) 0 0 0 0 0)))
;;                      )
;;                 (setq mbox-alist
;;                       (cons
;;                        (cons mbox-name (list time-stamp data))
;;                        mbox-alist
;;                        )
;;                       )
;;                 ))
;;             (setq ret-alist (cons (cons case mbox-alist) ret-alist))
;;             )
;;         )
;;       )
;;     ret-alist
;;     )
;;   )


;; 启动mew时会调用：mew-init-p ＝nil
;; Z 时会调用：mew-init-p ＝t  
;;(defun mew-mbox-imap-status-update ()
;;  (mew-mail-sum-msg "mew-mbox-imap-status-update called with %s" mew-init-p))
;; (let (tmp-alist)
;;   (mew-mail-sum-imap-cache-passwd)
;;   (if (not mew-init-p)
;;       ; mew启动时,只需要装载cache
;;       (progn 
;;         (mew-mail-sum-msg "mew-mail-sum-imap-status-update called at init")
;;         (mew-mail-sum-imap-load-mbox-alist)
;;         (if (not mew-mail-sum-imap-mbox-alist)
;;             (setq mew-mail-sum-imap-mbox-alist
;;                   (mew-mail-sum-imap-rebuild-mbox-alist)))
;;         (mew-mail-sum-imap-save-mbox-alist))
;;     ; Z 后，配置可能有更新需要merge配置
;;     (progn
;;       (mew-mail-sum-msg "mew-mail-sum-imap-status-update called after init")        
;;       (setq tmp-alist (mew-mail-sum-imap-rebuild-mbox-alist))
;;       (mew-mail-sum-imap-merge-mbox-alist tmp-alist)
;;       (mew-mail-sum-imap-save-mbox-alist)
;;       )
;;     )))

;; (defun mew-mail-sum-dump-mbox-list ()
;;   (interactive)
;;   (let (case mbox)
;;     (dolist (case mew-mail-sum-imap-mbox-alist)
;;       (message "case %s:" (car case))
;;       (dolist (mbox (cdr case))
;;         (let* ((val (cdr mbox))
;;                (time-stamp (nth-value 0 val))
;;                (data (nth-value 1 val))
;;                )
;;           (message "   %s --> %s:%s %s:(%d %d %d %d %d %d)"
;;                    (car mbox)
;;                    (car time-stamp)
;;                    (if (cdr time-stamp)
;;                        (format-time-string "%Y-%m-%d %H:%M:%S" (cdr time-stamp))
;;                      "N/A")
;;                    (car data)
;;                    (nth-value 0 (cdr data))
;;                    (nth-value 1 (cdr data))
;;                    (nth-value 2 (cdr data))
;;                    (nth-value 3 (cdr data))
;;                    (nth-value 4 (cdr data))
;;                    (nth-value 5 (cdr data))
;;                    ))))))

;; ;; 缓存password，如果已经缓存，则不操作
;; (defun mew-mail-sum-imap-cache-passwd-by-case (case)
;;   (let* ((prompt (format "IMAP password (%s): " (mew-imap-passtag2 case)))
;;          (pass (mew-input-passwd prompt (mew-imap-passtag2 case))))
;;     pass))

;; ;; 缓存所有passwd
;; (defun mew-mail-sum-imap-cache-passwd ()
;;   (setq mew-mail-sum-imap-passwd-alist nil)
;;   (dolist (case mew-config-cases)
;;     (if (eq (mew-mailbox-type case) 'imap)
;;         (mew-mail-sum-imap-cache-passwd-by-case case))
;;     )
;;   )


;; 获取缓存的password，如果缓存没有，提示用户输入
;; (defun mew-mail-sum-imap-get-cached-passwd (case)
;;   (let (passwd)
;;     (setq passwd (mew-passwd-get-passwd (mew-imap-passtag2 case)))
;;     (if (null passwd)
;;         (setq passwd (mew-mail-sum-imap-cache-passwd-by-case case)))
;;     passwd))

;; ;; 生成imap checker进程名
;; (defun mew-mail-sum-imap-encode-process-name (case mbox)
;;   (concat "*" mew-mail-sum-imap-process-name "/" case ":" mbox "*"))

;; ;; 非常简单的信号量
;; (defun mew-mail-sum-imap-checker-process-inc (proc-name &optional override-max)
;;   (mew-mail-sum-msg "mew-mail-sum-imap-checker-process-inc %s" proc-name)
;;   (if (or override-max (< (length mew-mail-sum-imap-checker-list)
;;                           mew-mail-sum-imap-checker-max-count))
;;       (if (not (member proc-name mew-mail-sum-imap-checker-list))
;;           (push proc-name mew-mail-sum-imap-checker-list)
;;         nil)
;;     nil)
;;   )

;; ;; 非常简单的信号量
;; (defun mew-mail-sum-imap-checker-process-dec (proc-name)
;;   (mew-mail-sum-msg "mew-mail-sum-imap-checker-process-dec %s" proc-name)
;;   (and (member proc-name mew-mail-sum-imap-checker-list)
;;        (setq mew-mail-sum-imap-checker-list
;;              (remove proc-name mew-mail-sum-imap-checker-list))))

;; ;; 设置进程属性值
;; (defun mew-mail-sum-imap-checker-get (proc key)
;;   (let ((r-key (concat mew-mail-sum-imap-checker-prefix key)))
;;     (process-get proc (intern r-key))))

;; ;; 获得进程属性值
;; (defun mew-mail-sum-imap-checker-set (proc key val)
;;   (let ((r-key (concat mew-mail-sum-imap-checker-prefix key)))
;;     (process-put proc (intern r-key) val)))

;; 更新一个imap邮箱的data数据，更新timestamp
;; MSGID|MESSAGES|RECENT|UIDNEXT|UIDVALIDITY|UNSEEN
;; (defun mew-mail-sum-imap-update-and-rotate (case mbox value)
;;   (mew-mail-sum-msg "update %s:%s -> %s" case mbox value)
;;   (let* (
;;          (mboxes (assoc case mew-mail-sum-imap-mbox-alist))
;;          (mbox (assoc mbox mboxes))
;;          (mbox-name (car mbox))
;;          (val (cdr mbox))
;;          (time-stamp (nth-value 0 val))
;;          (data (nth-value 1 val)))
;;     (setf (cdr time-stamp) (current-time))
;;     (setf (cdr data) (append (list (mew-mail-sum-imap-load-msgid case mbox-name)) value))
;;     )
;;   )

;; (defun mew-mail-sum-imap-parse-checker-output (output)
;;   (mew-mail-sum-msg "mew-mail-sum-imap-parse-checker-output called %s" output)
;;   (let ((val (split-string output "[ ]" t) ))
;;     (setq messages (string-to-number (nth-value 0 val)))
;;     (setq recent (string-to-number (nth-value 1 val)))
;;     (setq uidnext (string-to-number (nth-value 2 val)))
;;     (setq uidvalidity (string-to-number (nth-value 3 val)))
;;     (setq unseen (string-to-number (nth-value 4 val)))
;;     (list messages recent uidnext uidvalidity unseen)))

;; ;; checker进程退出
;; (defun mew-mail-sum-imap-update-mbox-sentinel (proc event)
;;   (mew-mail-sum-msg "[sentinel %s] return %s" proc event)
;;   (mew-mail-sum-imap-checker-process-dec (process-name proc))
;;   (let ((case (mew-mail-sum-imap-checker-get proc "case"))
;;         (mbox (mew-mail-sum-imap-checker-get proc "mbox"))
;;         (output (mew-mail-sum-imap-checker-get proc "output"))
;;         (proc-status (process-status proc))
;;         (proc-exit-code (process-exit-status proc)))
;;     (mew-mail-sum-msg "[sentinel %s] '%s:%s' output '%s'" proc case mbox output)
;;     (cond
;;      ((or (eq proc-status 'run) (eq proc-status 'stop))
;;       (progn
;;         (mew-mail-sum-msg "[sentinel %s] still running or stoped, kill it" proc)
;;         (kill-process proc)))
;;      ((eq proc-status 'signal)
;;       (progn
;;         (mew-mail-sum-msg "[sentinel %s] killed by signal %d" proc proc-exit-code)))
;;      ((eq proc-status 'exit)
;;       (progn
;;         (mew-mail-sum-msg "[sentinel %s] exit with status %d" proc proc-exit-code)
;;         (if (= 0 proc-exit-code)
;;             (let* ((msgid (mew-mail-sum-imap-load-msgid case mbox))
;;                    (val (mew-mail-sum-imap-parse-checker-output output)) )
;;               (mew-mail-sum-imap-update-and-rotate case mbox val))))))))

;; ;; 获取checker进程的输出
;; (defun mew-mail-sum-imap-update-mbox-filter (proc string)
;;   (mew-mail-sum-msg "[filter %s] return %s" proc string)
;;   (let ((outval  (mew-mail-sum-imap-checker-get proc "output")))
;;     (setq outval (concat outval string))
;;     (mew-mail-sum-imap-checker-set proc "output" outval)))

;;

;; 启动一个checker进程
;; (defun mew-mail-sum-imap-update-mbox (case mbox &optional override-max)
;;   (mew-mail-sum-msg "mew-mail-sum-imap-update-mbox-count called %s %s" case mbox override-max)
;;   (let* ((process-connection-type nil)
;;          (proc-path mew-mail-sum-imap-checker-proc)
;;          (server (mew-imap-server case))
;;          (port (mew-imap-port case))
;;          (sslp (mew-imap-ssl case))
;;          (sslport (mew-imap-ssl-port case))
;;          (user (mew-imap-user case))
;;          (passwd (mew-mail-sum-imap-get-cached-passwd case))
;;          (mailbox (mew-imap-utf-7-encode-string
;;                    (mew-imap-bnm-to-mailbox mbox)))
;;          (proc-name (mew-mail-sum-imap-encode-process-name case mbox))
;;          (process))
;;     (if (numberp port)
;;         (setq port (number-to-string port)))
;;     (if (numberp sslport)
;;         (setq sslport (number-to-string sslport)))
;;     (if (null passwd)
;;         (mew-mail-sum-msg "passwd is nil")
;;       (if (not (mew-mail-sum-imap-checker-process-inc proc-name override-max))
;;           (mew-mail-sum-msg "max number of checker process reached...")
;;         (progn
;;           (setq process (condition-case err
;;                             (start-process
;;                              proc-name
;;                              nil
;;                              proc-path
;;                              "-s"
;;                              server
;;                              "-p"
;;                              (if sslp sslport port)
;;                              "-u"
;;                              user
;;                              "-w"
;;                              passwd
;;                              "-m"
;;                              mailbox
;;                              "-S"
;;                              (if sslp "yes" "no")
;;                              )
;;                           (error (progn
;;                                    (mew-mail-sum-msg "can not start imap checker: %s"
;;                                                      (error-message-string err))
;;                                    (mew-mail-sum-imap-checker-process-dec proc-name)
;;                                    nil))))
;;           (if process
;;               (progn
;;                 (set-process-sentinel process 'mew-mail-sum-imap-update-mbox-sentinel)
;;                 (set-process-filter process 'mew-mail-sum-imap-update-mbox-filter)
;;                 (mew-mail-sum-imap-checker-set process "case" case)
;;                 (mew-mail-sum-imap-checker-set process "mbox" mbox)
;;                 (process-send-string process "START\n"))))))))


;; (defun mew-mail-sum-imap-get-flag (case mbox)
;;   (let ((proc-name (mew-mail-sum-imap-encode-process-name case mbox)))
;;         (if (member proc-name mew-mail-sum-imap-checker-list)            
;;         "*" "")))

;; (defun mew-mail-sum-imap-get-mail-count (data)
;;   (let ((msgid (nth-value 0 data))
;;         (messages (nth-value 1 data))
;;         (uidnext (nth-value 3 data))
;;         ret)
;;     ;; messages是0 或者msgid >= messages
;;     (if (or (equal 0 messages) (>= msgid messages))
;;         (setq ret 0) ;; 显示为0
;;       (progn
;;         (if (equal 0 msgid) ;; 如果msgid == 0
;;             (setq ret messages) ;; 显示messages
;;           (setq ret (- (- uidnext msgid) 1)))
;;         )
;;       )
;;     ret
;;     )
;;   )

;; 获取mbox列表
;; (defun mew-mail-sum-imap-generate-entries (case-prefix mbox-prefix no-zero-na)
;;   (mew-mail-sum-msg "mew-mail-sum-imap-generate-entries called case-prefix:'%s' mbox-prefix:'%s'" case-prefix mbox-prefix)
;;   (if (not (null mbox-prefix))
;;       (setq mbox-prefix (concat "%" mbox-prefix)))
;;   (let (entries skip)
;;     (dolist (case mew-mail-sum-imap-mbox-alist)
;;       (if (or (null case-prefix) (string-prefix-p case-prefix (car case)))
;;           (dolist (mbox (cdr case))
;;             (if (or (null mbox-prefix) (string-prefix-p mbox-prefix (car mbox)))
;;                 (let* ((case-name (car case))
;;                        (mbox-name (car mbox))
;;                        (case-name (car case))
;;                        (val (cdr mbox))
;;                        (time-stamp (nth-value 0 val))
;;                        (data (nth-value 1 val))
;;                        (mail-str
;;                         (if (cdr time-stamp)
;;                             (format "%d" (mew-mail-sum-imap-get-mail-count (cdr data)))
;;                           "???"))
;;                        (time-stamp-str
;;                         (if (cdr time-stamp)
;;                             (format-time-string "%Y%m%d %H:%M:%S" (cdr time-stamp)) "N/A"))
;;                        )

;;                   (setq skip (and no-zero-na (or (string= time-stamp-str "N/A") (string= mail-str "0") (string= mail-str "???")))) 

;;                   (if (not skip)
;;                       (push 
;;                        (list
;;                         (list case-name  mbox-name) ; key
;;                         (vector                                 ; value
;;                          (mew-mail-sum-imap-get-flag case-name mbox-name) ; U
;;                          "%"                                              ; P
;;                          time-stamp-str ; LAST-UPDATE
;;                          mail-str ; MAIL
;;                          case-name                                        ; CASE
;;                          mbox-name                                        ; MBOX
;;                          ))
;;                        entries))

;;                   )))))entries))

(provide 'mew-mbox-imap)
