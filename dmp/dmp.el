;;o dummy 音乐播放器
;; 以一个m3u格式的文件为媒体库的
;; 及其简单的表格界面的音乐播放器

(require 'dmp-msg)
(require 'dmp-player)
(require 'dmp-playlist)
(require 'dmp-info)

(defconst dmp-buffer-name "*Music*")


;; 元数据格式，这部分数据不保存在硬盘上
;; 以媒体文件的绝对路径为key的alist
;; (("abs-file-path" . [xxx]) .. .. )
;; play-flag :  "" or "=>"
;; mark : *|P|D|U
;; type  : 文件类型 MP3 WAV OGG
;; title : 歌曲标题
;; author: 歌曲作者
;; album : 专辑
;; track : 专辑里的歌曲序号
;; year  : 年代
;; genre : 类型
;; duration: 时长
(defvar dmp-table-entries-alist nil
  "界面元数据")

(defvar dmp-table-entries-mutex nil
  "保护界面元数据的锁")

;; 注意！！！
;; 所有需要同时访问dmp-table-entries-alist和dmp-playlist-alist的操作
;; 一定是先获得dmp-table-entries-mutex再获得dmp-playlist-mutex
;; 否则可能会死锁！

(defun dmp-buffers-revert () nil)

(define-derived-mode dmp-table-mode tabulated-list-mode "DMP Music Mode"
  "DMP table interface major mode"
  (add-hook 'tabulated-list-revert-hook 'dmp-buffers-revert nil t)
  nil)

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
(define-key dmp-table-mode-map  "al"  'dmp-table-add-filelist)
(define-key dmp-table-mode-map  "af"  'dmp-table-add-file)
(define-key dmp-table-mode-map  (kbd "C-x S") 'dmp-table-save-playlist)

(defun dmp-table-save-playlist ()
  "保存播放列表"
  (interactive)
  (dmp-playlist-save))

(defun dmp-table-refresh()
  "刷新界面"
  (interactive)
  (with-mutex dmp-table-entries-mutex
    (tabulated-list-print)))

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

(defun dmp-table-add-filelist(filelist)
  "从文件列表添加歌曲到playlist"
  (interactive "fselect playlist file: ")
  (with-mutex dmp-table-entries-mutex
    (when (dmp-playlist-append-filelist filelist)
      (dmp-table-recreate-entries t)))
  (dmp-info-start-batch))

(defun dmp-table-add-file(file)
  "从单个文件添加歌曲到playlist"
  (interactive "fselect media file: ")
  (with-mutex dmp-table-entries-mutex
    (when (dmp-playlist-append-file file)
      (dmp-table-create-entries t)))
  (dmp-info-start-batch))


(defun dmp-table-add-directory(directory)
  "从目录添加歌曲到playlist"
  (interactive "Dselect directory: ")
  (with-mutex dmp-table-entries-mutex
    (when (dmp-playlist-append-directory directory)
      (dmp-table-recreate-entries t)))
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
      (tabulated-list-print t t)
      (setq player-list (seq-filter 'dmp-table-entry-filter-P-mark dmp-table-entries-alist))
      (dmp-playlist-set-P (mapcar 'car player-list)))
    (dmp-player-set-playlist (mapcar 'car player-list))))

(defun dmp-table-mark ()
  "设置标记 *"
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when id
      (with-mutex dmp-table-entries-mutex
        (dmp-table-set-entry-*-mark (assoc id dmp-table-entries-alist 'string=))
        (tabulated-list-print t t)
        (next-line)))))

(defun dmp-table-unmark ()
  "清除标记 * D"
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when id
      (with-mutex dmp-table-entries-mutex
        (dmp-table-clear-entry-*-mark (assoc id dmp-table-entries-alist 'string=))
        (dmp-table-clear-entry-D-mark (assoc id dmp-table-entries-alist 'string=))
        (tabulated-list-print t t)
        (next-line)))))

(defun dmp-table-unmark-all ()
  "清除所有标记 * D"
  (interactive)
  (with-mutex dmp-table-entries-mutex
    (mapcar 'dmp-table-clear-entry-*-mark dmp-table-entries-alist)
    (mapcar 'dmp-table-clear-entry-D-mark dmp-table-entries-alist)
    (tabulated-list-print t t)))

(defun dmp-table-mark-to-delete ()
  "* => D"
  (interactive)
  (with-mutex dmp-table-entries-mutex
    (mapcar 'dmp-table-trans-entry-*-D-mark dmp-table-entries-alist)
    (tabulated-list-print t t)))


(defun dmp-table-mark-remove-delete ()
  "* D => ' '"
  (interactive)
  (with-mutex dmp-table-entries-mutex
    (mapcar 'dmp-table-trans-entry-*-uD-mark dmp-table-entries-alist)
    (tabulated-list-print t t)))

(defun dmp-table-mark-delete()
  "' ' => D"
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when id
      (with-mutex dmp-table-entries-mutex
        (dmp-table-set-entry-D-mark (assoc id dmp-table-entries-alist 'string=))
        (tabulated-list-print t t)
        (next-line)))))

(defun dmp-table-mark-to-add-player ()
  "* => P"
  (interactive)
  (let ((player-list))
    (with-mutex dmp-table-entries-mutex
      (mapcar 'dmp-table-trans-entry-*-P-mark dmp-table-entries-alist)
      (tabulated-list-print t t)
      (setq player-list (seq-filter 'dmp-table-entry-filter-P-mark dmp-table-entries-alist))
      (dmp-playlist-set-P (mapcar 'car player-list)))
    (dmp-player-set-playlist (mapcar 'car player-list))))


(defun dmp-table-mark-remove-player ()
  "* P => ' '"
  (interactive)
  (let ((player-list))
    (with-mutex dmp-table-entries-mutex
      (mapcar 'dmp-table-trans-entry-*-uP-mark dmp-table-entries-alist)
      (tabulated-list-print t t)
      (setq player-list (seq-filter 'dmp-table-entry-filter-P-mark dmp-table-entries-alist))
      (dmp-playlist-set-P (mapcar 'car player-list)))
    (dmp-player-set-playlist (mapcar 'car player-list))))

(defun dmp-table-excute-delete ()
  "D => delete"
  (interactive)
  (let ((delete-list))
    (with-mutex dmp-table-entries-mutex
      (setq delete-list (seq-filter 'dmp-table-entry-filter-D-mark dmp-table-entries-alist))
      (dmp-playlist-remove-entries (mapcar 'car delete-list))
      (dmp-table-recreate-entries t))))

(seq-filter 'dmp-table-entry-filter-D-mark dmp-table-entries-alist)

;;;;;;;;;;;;;私有函数

(defun dmp-table-init()
  (setq dmp-table-entries-mutex (make-mutex "dmp-table-entries-mutex")))

;;;;;; mark: *|P|D|U
(defun dmp-table-set-entry-*-mark (entry)
  "设置某一个条目的 * mark"
  (setf (aref (aref (cadr entry) 1 ) 0) ?* ))

(defun dmp-table-clear-entry-*-mark (entry)
  "清除某一个条目的 * mark"
  (setf (aref (aref (cadr entry) 1 ) 0) ?  ))

(defun dmp-table-set-entry-D-mark (entry)
  "设置某一个条目的 D mark"
  (setf (aref (aref (cadr entry) 1 ) 2) ?D ))

(defun dmp-table-clear-entry-D-mark (entry)
  "清除某一个条目的 D mark"
  (setf (aref (aref (cadr entry) 1 ) 2) ?  ))

(defun dmp-table-set-entry-P-mark (entry)
  "设置某一个条目的 P mark"
  (setf (aref (aref (cadr entry) 1 ) 1) ?P ))

(defun dmp-table-clear-entry-P-mark (entry)
  "清除某一个条目的 P mark"
  (setf (aref (aref (cadr entry) 1 ) 1) ?  ))

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
  (setf (aref (aref (cadr entry) 1 ) 3) ?  ))

(defun dmp-table-trans-entry-*-D-mark (entry)
  "将某个条目的*=>D"
  (let ((mark (aref (cadr entry) 1 )))
    (when (equal (aref mark 0) ?*)
      (setf (aref mark 0) ? )
      (setf (aref mark 2) ?D))))
  
(defun dmp-table-trans-entry-*-uD-mark (entry)
  "将某个条目的*D=> "
  (let ((mark (aref (cadr entry) 1 )))
    (when (and (equal (aref mark 0) ?*) (equal (aref mark 2) ?D))
      (setf (aref mark 0) ? )
      (setf (aref mark 2) ? ))))

(defun dmp-table-trans-entry-*-P-mark (entry)
  "将某个条目的*=>P"
  (let ((mark (aref (cadr entry) 1 )))
    (when (equal (aref mark 0) ?*)
      (setf (aref mark 0) ? )
      (setf (aref mark 1) ?P))))

(defun dmp-table-trans-entry-*-uP-mark (entry)
  "将某个条目的*P=> "
  (let ((mark (aref (cadr entry) 1 )))
    (when (and (equal (aref mark 0) ?*) (equal (aref mark 1) ?P))
      (setf (aref mark 0) ? )
      (setf (aref mark 1) ? ))))

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
  "重新从硬盘装载playlist，并刷新界面"
  (dmp-player-stop)
  (dmp-info-cancel-all)
  (with-mutex dmp-table-entries-mutex
    (when (dmp-playlist-load)
      (dmp-table-recreate-table)
      (dmp-table-recreate-entries))))

(defun dmp-table-trans-playlist-entry (entry)
  "转换playlist的元数据到一条dmp-table的元数据"
  (let* ((id (car entry))
        (data (cdr entry))
        (play-flag "  ")
        (mark (format " %s  " (if (aref data 0) "P" " ")))
        (info-valid (aref data 1))
        (type (if info-valid (aref data 2) "???"))
        (title (if info-valid (aref data 3) "???"))
        (author (if info-valid (aref data 4) "???"))
        (album (if info-valid (aref data 5) "???"))
        (track (if info-valid (aref data 6) "???"))
        (year (if info-valid (aref data 7) "???"))
        (genre (if info-valid (aref data 8) "???"))
        (duration (if info-valid (aref data 9) "???")))
    (list id (vector play-flag mark type title author album track year genre duration))))

(defun dmp-table-merge-entries (new-entries)
  "将dmp-table-entries-alist中的play-flag和mark merge到new-entries，忽略new-entries中没有的条目"
  (mapcar (lambda (item)
            (let ((entry)
                  (data (cadr item)))
              (setq entry (assoc (car item) dmp-table-entries-alist 'string=))
              (when entry
                (setf (aref data 0) (aref (cadr entry) 0))
                (setf (aref data 1) (aref (cadr entry) 1))))
            item)
          new-entries))

(defun dmp-table-recreate-entries (&optional merge)
  "生成dmp-table-entries, 如果merge为非nil，保留旧dmp-table-entries-alist中的play-flag和mark"
  (let ((new-entries))
    (setq new-entries (mapcar 'dmp-table-trans-playlist-entry dmp-playlist-alist))
    (if merge
        (setq dmp-table-entries-alist (dmp-table-merge-entries new-entries))
      (setq dmp-table-entries-alist new-entries))
    (setq tabulated-list-entries dmp-table-entries-alist)
    (tabulated-list-print t t)))

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
          (dmp-table-mode)
          (dmp-table-init)
          (dmp-playlist-init)
          (dmp-info-init)
          (dmp-player-init)
          (dmp-table-reload)
          (dmp-info-start-batch)
          (with-mutex dmp-table-entries-mutex
            (setq player-list (seq-filter 'dmp-table-entry-filter-P-mark dmp-table-entries-alist)))
          (dmp-player-set-playlist (mapcar 'car player-list)))))
    (pop-to-buffer buffer)))



(defun dmp-table-recreate-table ()
  "DMP界面定义"
  (setq tabulated-list-format
	    (vector
         ;; 播放中=>
         '("P" 4 nil)
         ;; 标志 */P/D/U
         '("F" 4 nil)
         ;; 类型：MP3/OGG/WAV
         '("TYPE" 4 nil :pad-right 0)
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
  (tabulated-list-init-header))

(provide 'dmp)
