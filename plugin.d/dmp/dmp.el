;; -*- lexical-binding: t; -*-
;; dummy 音乐播放器
;; 以一个m3u格式的文件为媒体库的
;; 及其简单的表格界面的音乐播放器

(require 'dmp-lisp)
(require 'dmp-msg)
(require 'dmp-player)
(require 'dmp-info)

(defconst dmp-buffer-name "*Music*")

(defface dmp-normal-face
  '((t (:inherit normal)))
  "正常条目使用的face")

(defface dmp-*-face
  '((t (:inherit normal :foreground "cyan")))
  "* 条目使用的face")

(defface dmp-P-face
  '((t (:inherit normal :foreground "yellow")))
  "P 条目使用的face")

(defface dmp-U-face
  '((t (:inherit normal :foreground "blue")))
  "U 条目使用的face")

(defface dmp-D-face
  '((t (:inherit normal :foreground "red")))
  "D 条目使用的face")

(defface dmp-B-face
  '((t (:inherit normal :foreground "gray")))
  "B 条目使用的face")



;; 硬盘元数据格式
;; 以媒体文件的绝对路径为key的alist
;; (("abs-file-path" . [xxx]) .. .. )
;; P     : in the player's job list
;; I     : 元数据 信息有效
;; G     : 文件完好
;; type  : 文件类型 MP3 OGG FLAC M4A
;; title : 歌曲标题
;; author: 歌曲作者
;; album : 专辑
;; track : 专辑里的歌曲序号
;; year  : 年代
;; genre : 类型
;; duration: 时长

;; 内存元数据格式，这部分数据和保存在硬盘上的元数据不完全相同
;; play-flag, mark 不保存
;; 以媒体文件的绝对路径为key的alist
;; (("abs-file-path"  [xxx]) .. .. )
;; play-flag :  "" or "=>"
;; mark : *|P|D|U 标记|待播放|待删除|更新中
;; type  : 文件类型 MP3 WAV OGG
;; title : 歌曲标题
;; author: 歌曲作者
;; album : 专辑
;; track : 专辑里的歌曲序号
;; year  : 年代
;; genre : 类型
;; duration: 时长
;; file_good: 文件损坏
;; info_good: 元数据有效
(defvar dmp-table-entries-alist nil
  "界面元数据")

(defvar dmp-table-entries-mutex nil
  "保护界面元数据的锁")

(defvar dmp-playlist-filename "~/Downloads/playlist.el"
  "播放列表的位置")

(defvar dmp-config-filename "~/Downloads/dmp-conf.el"
  "配置文件位置")

(defvar dmp-config-alist
  '(
    (tabulated-list-sort-key . ("TITLE" . t))
    )
  "配置")


(define-derived-mode dmp-table-mode tabulated-list-mode "DMP Music Mode"
  "DMP table interface major mode")

(define-key dmp-table-mode-map  (kbd "SPC") 'dmp-table-play-pause)
(define-key dmp-table-mode-map  "s"  'dmp-table-stop)
(define-key dmp-table-mode-map  "t"  'dmp-table-toggle-loop-style)
(define-key dmp-table-mode-map  "+"  'dmp-table-volume-up)
(define-key dmp-table-mode-map  "-"  'dmp-table-volume-down)
(define-key dmp-table-mode-map  ">"  'dmp-table-track-next)
(define-key dmp-table-mode-map  "<"  'dmp-table-track-prev)
(define-key dmp-table-mode-map  (kbd "RET") 'dmp-table-play-current)
(define-key dmp-table-mode-map  "p"  'dmp-table-toggle-in-player)
(define-key dmp-table-mode-map  "*"  'dmp-table-mark)
(define-key dmp-table-mode-map  "u"  'dmp-table-unmark)
(define-key dmp-table-mode-map  "U"  'dmp-table-unmark-all)
(define-key dmp-table-mode-map  "md" 'dmp-table-mark-to-delete)
(define-key dmp-table-mode-map  (kbd "C-u md") 'dmp-table-mark-remove-delete)
(define-key dmp-table-mode-map  "d"  'dmp-table-mark-delete)
(define-key dmp-table-mode-map  "x"  'dmp-table-excute-delete)
(define-key dmp-table-mode-map  "mp" 'dmp-table-mark-to-add-player)
(define-key dmp-table-mode-map  (kbd "C-u mp") 'dmp-table-mark-remove-player)
(define-key dmp-table-mode-map  "g"  'dmp-table-refresh)
(define-key dmp-table-mode-map  (kbd "C-x G") 'dmp-table-rebuild-all-info)
(define-key dmp-table-mode-map  "G"  'dmp-table-rebuild-info)
(define-key dmp-table-mode-map  "af"  'dmp-table-add-file)
(define-key dmp-table-mode-map  "ad"  'dmp-table-add-directory)
(define-key dmp-table-mode-map  (kbd "C-x S") 'dmp-table-save)

(defun dmp-table-save ()
  "保存播放列表"
  (interactive)
  (with-mutex dmp-table-entries-mutex
    (dmp-table-save-entries))
  (dmp-save-config))

(defun dmp-table-refresh()
  "刷新界面"
  (interactive)
  (with-mutex dmp-table-entries-mutex
    (tabulated-list-print t)))

(defun dmp-table-rebuild-all-info ()
  "重新更新一遍所有元数据"
  (interactive)
  (dmp-info-start-batch nil t))

(defun dmp-table-rebuild-info ()
  "重新更新所在条目元数据"
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when id
      (dmp-info-start-oneshot id t))))

(defun dmp-table-add-file(file)
  "从单个文件添加歌曲"
  (interactive "fselect media file: ")
  (with-mutex dmp-table-entries-mutex
    (dmp-table-append-file file)
    (tabulated-list-print t))
  (dmp-info-start-batch))


(defun dmp-table-add-directory(directory)
  "从目录添加歌曲"
  (interactive "Dselect directory: ")
  (with-mutex dmp-table-entries-mutex
    (dmp-table-append-directory directory)
    (tabulated-list-print t))
  (dmp-info-start-batch))

(defun dmp-table-play-pause ()
  "播放/暂停当前歌单(P条目)"
  (interactive)
  (dmp-player-play-pause))

(defun dmp-table-stop ()
  "停止播放当前曲目/歌单"
  (interactive)
  (dmp-player-stop))

(defun dmp-play-current ()
  "播放当前选中歌曲，但是不加入歌单"
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when id
      (dmp-player-play id))))


(defun dmp-table-toggle-loop-style ()
  "切换播放方式：单次顺序/单次随机/循环顺序/循环随机"
  (interactive)
  (dmp-player-toggle-loop-style))

(defun dmp-table-volume-up()
  "声音增大"
  (interactive)
  (dmp-player-volume-up))

(defun dmp-table-volume-up()
  "声音减少"
  (interactive)
  (dmp-player-volume-down))

(defun dmp-table-track-next ()
  "下一首歌曲"
  (interactive)
  (dmp-player-track-next))

(defun dmp-table-track-prev ()
  "上一首歌曲"
  (interactive)
  (dmp-player-track-prev))

(defun dmp-table-toggle-in-player ()
  "toggle是否在player中 P => ' ' or ' ' => P"
  (interactive)
  (let ((player-list)
        (id (tabulated-list-get-id)))
    (with-mutex dmp-table-entries-mutex
      (dmp-table-toggle-entry-P-mark (assoc id dmp-table-entries-alist 'string=))
      (tabulated-list-print t)
      (setq player-list (seq-filter 'dmp-table-entry-filter-P-mark dmp-table-entries-alist)))
    (dmp-player-set-playlist (mapcar 'car player-list))))

(defun dmp-table-mark ()
  "设置标记 *"
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when id
      (with-mutex dmp-table-entries-mutex
        (dmp-table-set-entry-*-mark (assoc id dmp-table-entries-alist 'string=))
        (tabulated-list-print t)
        (next-line)))))

(defun dmp-table-unmark ()
  "清除标记 * D"
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when id
      (with-mutex dmp-table-entries-mutex
        (dmp-table-clear-entry-*-mark (assoc id dmp-table-entries-alist 'string=))
        (dmp-table-clear-entry-D-mark (assoc id dmp-table-entries-alist 'string=))
        (tabulated-list-print t)
        (next-line)))))

(defun dmp-table-unmark-all ()
  "清除所有标记 * D"
  (interactive)
  (with-mutex dmp-table-entries-mutex
    (mapcar 'dmp-table-clear-entry-*-mark dmp-table-entries-alist)
    (mapcar 'dmp-table-clear-entry-D-mark dmp-table-entries-alist)
    (tabulated-list-print t)))

(defun dmp-table-mark-to-delete ()
  "* => D"
  (interactive)
  (with-mutex dmp-table-entries-mutex
    (mapcar 'dmp-table-trans-entry-*-D-mark dmp-table-entries-alist)
    (tabulated-list-print t)))


(defun dmp-table-mark-remove-delete ()
  "* D => ' '"
  (interactive)
  (with-mutex dmp-table-entries-mutex
    (mapcar 'dmp-table-trans-entry-*-uD-mark dmp-table-entries-alist)
    (tabulated-list-print t)))

(defun dmp-table-mark-delete()
  "' ' => D"
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when id
      (with-mutex dmp-table-entries-mutex
        (dmp-table-set-entry-D-mark (assoc id dmp-table-entries-alist 'string=))
        (tabulated-list-print t)
        (next-line)))))

(defun dmp-table-mark-to-add-player ()
  "* => P"
  (interactive)
  (let ((player-list))
    (with-mutex dmp-table-entries-mutex
      (mapcar 'dmp-table-trans-entry-*-P-mark dmp-table-entries-alist)
      (tabulated-list-print t)
      (setq player-list (seq-filter 'dmp-table-entry-filter-P-mark dmp-table-entries-alist)))
    (dmp-player-set-playlist (mapcar 'car player-list))))


(defun dmp-table-mark-remove-player ()
  "* P => ' '"
  (interactive)
  (let ((player-list))
    (with-mutex dmp-table-entries-mutex
      (mapcar 'dmp-table-trans-entry-*-uP-mark dmp-table-entries-alist)
      (tabulated-list-print t)
      (setq player-list (seq-filter 'dmp-table-entry-filter-P-mark dmp-table-entries-alist)))
    (dmp-player-set-playlist (mapcar 'car player-list))))

(defun dmp-table-excute-delete ()
  "D => delete"
  (interactive)
  (let ((delete-list))
    (with-mutex dmp-table-entries-mutex
      (setq delete-list (seq-filter 'dmp-table-entry-filter-D-mark dmp-table-entries-alist))
      (dmp-table-remove-entries (mapcar 'car delete-list)))))

(seq-filter 'dmp-table-entry-filter-D-mark dmp-table-entries-alist)

;;;;;;;;;;;;;私有函数

(defun dmp-table-init()
  (unless dmp-table-entries-mutex
    (setq dmp-table-entries-mutex (make-mutex "dmp-table-entries-mutex"))))

;;;;;; mark: *|P|D|U
(defun dmp-table-set-entry-*-mark (entry)
  "设置某一个条目的 * mark"
  (setf (aref (aref (cadr entry) 1 ) 0) ?* ))

(defun dmp-table-clear-entry-*-mark (entry)
  "清除某一个条目的 * mark"
  (setf (aref (aref (cadr entry) 1 ) 0) ?\s ))

(defun dmp-table-set-entry-D-mark (entry)
  "设置某一个条目的 D mark"
  (setf (aref (aref (cadr entry) 1 ) 2) ?D ))

(defun dmp-table-clear-entry-D-mark (entry)
  "清除某一个条目的 D mark"
  (setf (aref (aref (cadr entry) 1 ) 2) ?\s ))

(defun dmp-table-set-entry-P-mark (entry)
  "设置某一个条目的 P mark"
  (setf (aref (aref (cadr entry) 1 ) 1) ?P ))

(defun dmp-table-clear-entry-P-mark (entry)
  "清除某一个条目的 P mark"
  (setf (aref (aref (cadr entry) 1 ) 1) ?\s ))

(defun dmp-table-toggle-entry-P-mark (entry)
  "toggle 一个条目的 P mark"
  (if (dmp-table-entry-filter-P-mark entry)
      (dmp-table-clear-entry-P-mark entry)
    (dmp-table-set-entry-P-mark entry)))

(defun dmp-table-set-entry-U-mark (entry)
  "设置某一个条目的 U mark"
  (setf (aref (aref (cadr entry) 1 ) 3) ?U ))

(defun dmp-table-clear-entry-U-mark (entry)
  "清除某一个条目的 U mark"
  (setf (aref (aref (cadr entry) 1 ) 3) ?\s))

(defun dmp-table-trans-entry-*-D-mark (entry)
  "将某个条目的*=>D"
  (let ((mark (aref (cadr entry) 1 )))
    (when (equal (aref mark 0) ?*)
      (setf (aref mark 0) ?\s)
      (setf (aref mark 2) ?D))))
  
(defun dmp-table-trans-entry-*-uD-mark (entry)
  "将某个条目的*D=> "
  (let ((mark (aref (cadr entry) 1 )))
    (when (and (equal (aref mark 0) ?*) (equal (aref mark 2) ?D))
      (setf (aref mark 0) ?\s)
      (setf (aref mark 2) ?\s))))

(defun dmp-table-trans-entry-*-P-mark (entry)
  "将某个条目的*=>P"
  (let ((mark (aref (cadr entry) 1 )))
    (when (equal (aref mark 0) ?*)
      (setf (aref mark 0) ?\s)
      (setf (aref mark 1) ?P))))

(defun dmp-table-trans-entry-*-uP-mark (entry)
  "将某个条目的*P=> "
  (let ((mark (aref (cadr entry) 1 )))
    (when (and (equal (aref mark 0) ?*) (equal (aref mark 1) ?P))
      (setf (aref mark 0) ?\s)
      (setf (aref mark 1) ?\s))))

(defun dmp-table-entry-filter-P-mark (entry)
  "如果某个条目有P mark 返回，否则返回nil"
  (when (equal (aref (aref (cadr entry) 1 ) 1) ?P) entry))

(defun dmp-table-entry-filter-D-mark (entry)
  "如果某个条目有D mark 返回，否则返回nil"
   (when (equal (aref (aref (cadr entry) 1 ) 2) ?D) entry))

(defun dmp-table-entry-filter-uD-mark (entry)
  "如果某个条目没有D mark 返回，否则返回nil"
   (unless (equal (aref (aref (cadr entry) 1 ) 2) ?D) entry))
;;;;;;;;;;;;;;;

(defun dmp-table-reload()
  "重新从硬盘装载元数据，并刷新界面"
  (dmp-player-stop)
  (dmp-info-cancel-all)
  (with-mutex dmp-table-entries-mutex
    (dmp-table-recreate-table)
    (dmp-table-load-entries)
    (tabulated-list-print t)))


(defun dmp-table-trans-table-entry-from-disk (entry)
  "转换硬盘元数据到内存元数据"
  (let* ((id (car entry))
        (data (cdr entry))
        (play-flag "  ")
        (mark (format " %s  " (if (aref data 0) "P" " ")))
        (info-good (aref data 1))
        (file-good (aref data 2))
        (type (if info-good (aref data 3) "???"))
        (title (aref data 4))
        (author (if info-good (aref data 5) "???"))
        (album (if info-good (aref data 6) "???"))
        (track (if info-good (aref data 7) "???"))
        (year (if info-good (aref data 8) "???"))
        (genre (if info-good (aref data 9) "???"))
        (duration (if info-good (aref data 10) "???")))
    (list id (vector play-flag mark type title author album track year genre duration file-good info-good))))

(defun dmp-table-trans-table-entry-to-disk (entry)
  "转换内存元数据到硬盘元数据"
  (let* ((id (car entry))
         (data (cadr entry))
         (mark (aref data 1))
         (P (equal (aref mark 1) ?P))
         (I (aref data 11))
         (G (aref data 10))
         (type (if I (aref data 2) nil))
         (title (aref data 3))
         (author (if I (aref data 4) nil))
         (album (if I (aref data 5) nil))
         (track (if I (aref data 6) nil))
         (year (if I (aref data 7) nil))
         (genre (if I (aref data 8)))
         (duration (if I (aref data 9) nil)))
    (cons id (vector P I G type title author album track year genre duration))))

  
(defun dmp-table-save-entries()
  "保存元数据到硬盘"
   (condition-case err
       (let ((dmp-list))
         (setq dmp-list (mapcar 'dmp-table-trans-table-entry-to-disk dmp-table-entries-alist))
         (dmp-lisp-save dmp-playlist-filename dmp-list t t)
         (message "Playlist %s (%d entries) saved!" dmp-playlist-filename (length dmp-table-entries-alist)))
     (dmp-log-error "dmp-table-save-entries failed %s" (error-message-string err))))


(defun dmp-table-load-entries()
  "装载元数据到硬盘"
   (condition-case err
       (let ((dmp-list))
         (setq dmp-playlist-alist (dmp-lisp-load dmp-playlist-filename))
         (setq dmp-table-entries-alist (mapcar 'dmp-table-trans-table-entry-from-disk dmp-playlist-alist))
         (message "Playlist %s (%d entries) loaded!" dmp-playlist-filename (length dmp-table-entries-alist)))
     (dmp-log-error "dmp-table-load-entries failed %s" (error-message-string err))))


(defun dmp-table-append-file-internal (file)
  "向列表中追加单个文件，无锁"
  (dmp-log-debug "adding file %s" file)
  (if (not (assoc file dmp-table-entries-alist))
      (setq dmp-table-entries-alist
            (append (list (list file (vector
                                    "   "    ;play-flag
                                    "    "   ;mark
                                    "???"    ;type
                                    (file-name-nondirectory file)    ;title
                                    "???"    ;author
                                    "???"    ;album
                                    "???"    ;track
                                    "???"    ;year
                                    "???"    ;genre
                                    "???"    ;duration
                                    nil      ;file_good
                                    nil      ;info_good
                                    )))
                    dmp-table-entries-alist))
    (dmp-log-warn "file %s" already exist file)))

(defun dmp-table-match-file-extensions (ext-list)
  "根据 EXT-LIST 生成匹配多个扩展名的正则表达式。"
  (concat "\\." (regexp-opt ext-list t) "\\'"))

(defun dmp-table-append-file (file)
  "添加文件，形成新条目"
  (if (and (file-regular-p file) (file-readable-p file))
      (dmp-table-append-file-internal file)
    (message "File %s not readable" file)))
 

(defun dmp-table-append-directory (directory)
  "添加目录，形成新条目"
  (if (file-directory-p directory)
      (let ((files
             (directory-files-recursively
              (expand-file-name directory)
              (dmp-table-match-file-extensions '("mp3" "flac" "m4a" "ogg")))))
        (dolist (f files)
          (dmp-table-append-file-internal f)))
    (message "Directory %s not readable or not exist" directory)))


(defun dmp-table-remove-entries (delete-list)
  "从dmp-table-entries-alist删除delete-list"
  (dolist (key delete-list)
    (setq dmp-table-entries-alist (assoc-delete-all key dmp-table-entries-alist))))

(defun dmp-table-open-buffer ()
  "打开DMP的主界面"
  (interactive)
  (dmp-log-debug "dmp-open-buffer called")
  (let* ((buffer-name dmp-buffer-name)
         (buffer (get-buffer buffer-name)))
    (unless buffer
      (setq buffer (generate-new-buffer buffer-name))
      (with-current-buffer buffer
        (let ((player-list))
          (dmp-load-config)
          (dmp-table-mode)
          (dmp-table-init)
          (dmp-info-init)
          (dmp-player-init)
          (dmp-table-reload)
          (dmp-info-start-batch)
          (with-mutex dmp-table-entries-mutex
            (setq player-list (seq-filter 'dmp-table-entry-filter-P-mark dmp-table-entries-alist)))
          (dmp-player-set-playlist (mapcar 'car player-list)))))
    (pop-to-buffer buffer)))

(defun dmp-table-generate-entries ()
  dmp-table-entries-alist)

(defun dmp-table-recreate-table ()
  "DMP界面定义"
  (setq tabulated-list-format
	    (vector
         ;; 播放中=>
         '("P" 4 nil)
         ;; 标志 */P/D/U
         '("F" 4 nil)
         ;; 类型：MP3/OGG/WAV
         '("TYPE" 4 t :pad-right 0)
         ;; Title
		 '("TITLE" 30 t)
         ;; Author
         '("AUTHOR" 12 t :left-align t)
         ;; Album
         '("ALBUM" 12 t :left-align t)
         ;; Track
         '("TRACK" 12 t :left-align t)
         ;; Year
         '("YEAR" 12 t :left-align t)
         ;; Genre
         '("GENRE" 12 t :left-align t)
         ;; Duration
         '("DURATION" 12 t :left-align t)
         ))
  (setq tabulated-list-use-header-line t)
  (setq tabulated-list-entries 'dmp-table-generate-entries)
  (setq tabulated-list-sort-key (alist-get 'tabulated-list-sort-key  dmp-config-alist))
  (tabulated-list-init-header)
  (tabulated-list-print t))

;;;;;;;;;配置文件
(defun dmp-load-config ()
  "装载配置文件"
  (condition-case err
      (when (and (file-regular-p dmp-config-filename) (file-readable-p dmp-config-filename))
        (setq dmp-config-alist (dmp-lisp-load dmp-config-filename))
        (message "Config file %s loaded!" dmp-config-filename))
    (dmp-log-error "dmp-load-config failed %s" (error-message-string err))))

(defun dmp-save-config ()
  "保存配置文件"
  (setf (alist-get 'tabulated-list-sort-key dmp-config-alist) tabulated-list-sort-key)
  (condition-case err
      (progn
        (dmp-lisp-save dmp-config-filename dmp-config-alist t t)
        (message "Config file %s saved!" dmp-config-filename))
    (dmp-log-error "dmp-save-config failed %s" (error-message-string err))))

(provide 'dmp)
