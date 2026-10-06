;; -*- lexical-binding: t; -*-
;; DMP playlist元数据和相关管理函数

;; 元数据格式
;; 以媒体文件的绝对路径为key的alist
;; (("abs-file-path" . [xxx]) .. .. )
;; P     : in the player's job list
;; I     : info 信息有效
;; type  : 文件类型 MP3 OGG FLAC M4A
;; title : 歌曲标题
;; author: 歌曲作者
;; album : 专辑
;; track : 专辑里的歌曲序号
;; year  : 年代
;; genre : 类型
;; duration: 时长
(defvar dmp-playlist-alist  nil
  "核心元数据，整个播放列表")

(defvar dmp-playlist-mutex nil
  "对核心元数据的锁")

(defvar dmp-lisp-max-length 20000000
  "默认元数据最大长度")

(defvar dmp-playlist-filename "~/Downloads/playlist.el"
  "播放列表的位置")

(defun dmp-playlist-init()
  "初始化"
  (setq dmp-playlist-mutex (make-mutex "dmp-playlist-mutex")))

(defun dmp-playlist-load ()
  "从硬盘装载playlist元数据"
  (let ((ret))
    (with-mutex dmp-playlist-mutex
      (condition-case err
          (progn
            (setq dmp-playlist-alist (dmp-lisp-load dmp-playlist-filename))
            (setq ret t))
        (dmp-log-error "dmp-playlist-load failed %s" (error-message-string err)))))
  ret)

(defun dmp-playlist-save ()
  "保存playlist元数据到硬盘"
  (with-mutex dmp-playlist-mutex
    (condition-case err
        (dmp-lisp-save dmp-playlist-filename dmp-playlist-alist t t)
      (dmp-log-error "dmp-playlist-save failed %s" (error-message-string err)))))

(defun dmp-playlist-append-file-internal (file)
  "向列表中追加单个文件，无锁"
  (when (and (file-readable-p file) (not (assoc file dmp-playlist-alist)))
    (setq dmp-playlist-alist
          (append (list (cons file (vector nil nil nil nil nil nil nil nil nil nil)))
                  dmp-playlist-alist))))

(defun dmp-playlist-append-file (file)
  "向列表中追加单个文件"
  (if (file-readable-p file)
      (with-mutex dmp-playlist-mutex
        (dmp-playlist-append-file-internal file)
        t)
    nil))

(defun dmp-playlist-match-extensions (ext-list)
  "根据 EXT-LIST 生成匹配多个扩展名的正则表达式。"
  (concat "\\." (regexp-opt ext-list t) "\\'"))

(defun dmp-playlist-append-directory (directory)
  "向列表中追加整个目录"
  (if (file-directory-p directory)
      (with-mutex dmp-playlist-mutex
        (let ((files
               (directory-files-recursively
                (expand-file-name directory)
                ( dmp-playlist-match-extensions '("mp3" "flac" "m4a" "ogg")))))
          (dolist (f files)
            (dmp-log-debug "adding file %s" f)
            (dmp-playlist-append-file-internal f)))
        t)
    nil))

(defun dmp-playlist-remove-entries (entries)
  "删除列表"
  (dmp-log-debug "dmp-playlist-remove-entries %s" entries)
  (with-mutex dmp-playlist-mutex
    (setq dmp-playlist-alist (seq-filter
                              (lambda (item)
                                (not (member (car item) entries)))
                              dmp-playlist-alist))))

(defun dmp-playlist-set-P (entries)
  "将id在entries中的条目的P设置，不在的清除"
  (dmp-log-debug "dmp-playlist-set-P %s" entries)
  (with-mutex dmp-playlist-mutex
    (mapcar (lambda (item)
              (if (member (car item) entries)
                  (setf (aref (cadr item) 0) t)
                (setf (aref (cadr item) 0) nil)))
            dmp-playlist-alist)))


;;;;;;;;;;;;;;私有函数

(defmacro dmp-frwlet (read write &rest body)
  ;; (declare (indent 2))
  `(let ((coding-system-for-read  ,read)
	     (coding-system-for-write ,write)
	     (format-alist nil)
	     (auto-image-file-mode nil)
	     (jka-compr-inhibit t))
     ,@body))
(put 'dmp-frwlet 'lisp-indent-function 2)

(defmacro dmp-ntake (n lst)
  `(if (> (length ,lst) ,n) (setcdr (nthcdr (1- ,n) ,lst) nil)))

(defun dmp-lisp-load (filename)
  "Load lisp from FILENAME"
  (let ((fullname (expand-file-name filename)))
    (if (file-readable-p fullname)
	    (with-temp-buffer
	      (dmp-frwlet 'utf-8-unix 'utf-8-unix
	        (insert-file-contents fullname))
	      (goto-char (point-min))
	      (condition-case nil
	          (read (current-buffer))
	        (error ()))))))

(defun dmp-lisp-save (filename lisp &optional nobackup unlimit)
  "Save LISP to FILENAME. LISP is truncated to dmp-lisp-max-length
by side-effect."
  (let* ((fullname (expand-file-name filename mew-conf-path))
	     (backname (concat fullname ".BAK"))
	     print-length print-level) ;; for Emacs 21
    (when (file-writable-p fullname)
      (if nobackup
	      (delete-file fullname)
	    (if (file-exists-p fullname)
	        (rename-file fullname backname 'override)))
      (when (and (not unlimit) (> (length lisp) dmp-lisp-max-length))
	    (setq lisp (copy-sequence lisp)) ;; no side effect
	    (dmp-ntake mew-lisp-max-length lisp))
      (with-temp-buffer
	    (if (> (length lisp) mew-lisp-max-length)
	        (print lisp (current-buffer))
	      (pp lisp (current-buffer)))
	    (dmp-frwlet 'utf-8-unix 'utf-8-unix
	      (write-region (point-min) (point-max) fullname nil 'no-msg))))))


(provide 'dmp-playlist)
