;; dummy 音乐播放器
;; 以一个m3u格式的文件为媒体库的
;; 及其简单的表格界面的音乐播放器

(require 'dmp-msg)
(require 'dmp-player)
(require 'dmp-playlist')
(require 'dmp-info)

(defconst dmp-buffer-name "*Music*")


;; 元数据格式，这部分数据不保存在硬盘上
;; 以媒体文件的绝对路径为key的alist
;; (("abs-file-path" . [xxx]) .. .. )
;; P :  "" or "=>"
;; F : *|P|D|U
;; type  : 文件类型 MP3 WAV OGG
;; title : 歌曲标题
;; author: 歌曲作者
;; album : 专辑
;; track : 专辑里的歌曲序号
;; year  : 年代
;; genre : 类型
;; duration: 时长
(defvar dmp-table-entries-alist nil
  "table的元数据")

(defvar dmp-table-entries-mutex nil)

(defun dmp-buffers-revert () nil)

(define-derived-mode dmp-table-mode tabulated-list-mode "DMP Music Mode"
  "DMP table interface major mode"
  (add-hook 'tabulated-list-revert-hook 'dmp-buffers-revert nil t)
  nil)

(define-key dmp-table-mode-map  "p"  'dmp-table-play-pause)
(define-key dmp-table-mode-map  "s"  'dmp-table-stop)
(define-key dmp-table-mode-map  "t"  'dmp-table-toggle-loop-style)
(define-key dmp-table-mode-map  "+"  'dmp-table-volume-up)
(define-key dmp-table-mode-map  "-"  'dmp-table-volume-down)
(define-key dmp-table-mode-map  ">"  'dmp-table-track-next)
(define-key dmp-table-mode-map  "<"  'dmp-table-track-prev)
(define-key dmp-table-mode-map  (kbd "RET") 'dmp-table-play-current)
(define-key dmp-table-mode-map  "*"  'dmp-table-mark)
(define-key dmp-table-mode-map  "u"  'dmp-table-unmark)
(define-key dmp-table-mode-map  "U"  'dmp-table-unmark-all)
(define-key dmp-table-mode-map  "md" 'dmp-table-mark-to-delete)
(define-key dmp-table-mode-map  "d"  'dmp-table-mark-delete)
(define-key dmp-table-mode-map  "x"  'dmp-table-excute-delete)
(define-key dmp-table-mode-map  "mp" 'dmp-table-mark-to-add-player)
(define-key dmp-table-mode-map  "mu" 'dmp-table-mark-to-remove-player)
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
  (when (dmp-playlist-append-filelist filelist)
    (with-mutex dmp-table-entries-mutex
      (with-mutex dmp-playlist-mutex
        (dmp-table-create-entries t))
      (tabulated-list-print))
    (dmp-info-start-batch)))

(defun dmp-table-add-file(file)
  "从单个文件添加歌曲到playlist"
  (interactive "fselect media file: ")
  (when (dmp-playlist-append-file filelist)
    (with-mutex dmp-table-entries-mutex
      (with-mutex dmp-playlist-mutex
        (dmp-table-create-entries t))
      (tabulated-list-print))
    (dmp-info-start-batch)))

  
(defun dmp-table-add-directory(directory)
  "从目录添加歌曲到playlist"
  (interactive "Dselect directory: ")
  (when (dmp-playlist-append-directory directory)
    (with-mutex dmp-table-entries-mutex
      (with-mutex dmp-playlist-mutex
        (dmp-table-create-entries t))
      (tabulated-list-print))
    (dmp-info-start-batch)))


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

(defun dmp-table-mark ()
  "设置标记 *"
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when id
      (with-mutex dmp-table-entries-mutex
        (dmp-table-set-entry-*-mark (assoc id dmp-table-entries-alist 'string=))))))

(defun dmp-table-unmark ()
  "清除标记 * D"
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when id
      (with-mutex dmp-table-entries-mutex
        (dmp-table-clear-entry-*-mark (assoc id dmp-table-entries-alist 'string=))
        (dmp-table-clear-entry-D-mark (assoc id dmp-table-entries-alist 'string=))))))

(defun dmp-table-unmark-all ()
  "清除所有标记 * D"
  (interactive)
  (with-mutex dmp-table-entries-mutex
    (mapcar 'dmp-table-clear-entry-*-mark dmp-table-entries-alist)
    (mapcar 'dmp-table-clear-entry-D-mark dmp-table-entries-alist)))

(defun dmp-table-mark-to-delete ()
  "* => D"
  (interactive)
  (with-mutex dmp-table-entries-mutex
    (mapcar 'dmp-table-trans-entry-*-D-mark dmp-table-entries-alist)))


(defun dmp-table-mark-delete()
  " => D"
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when id
      (with-mutex dmp-table-entries-mutex
        (dmp-table-set-entry-D-mark (assoc id dmp-table-entries-alist 'string=))))))

(defun dmp-table-mark-to-add-player ()
  "* => P"
  (interactive)
  (with-mutex dmp-table-entries-mutex
    (mapcar 'dmp-table-trans-entry-*-D-mark dmp-table-entries-alist)))


(defun dmp-table-mark-to-remove-player ()
  "* => "
  (interactive)
  (with-mutex dmp-table-entries-mutex
    (mapcar 'dmp-table-clear-entry-*-mark dmp-table-entries-alist)))

(defun dmp-table-excute-delete ()
  "D => delete"
  (interactive)
  nil)


;;;;;;;;;;;;;私有函数

(defun dmp-table-set-entry-*-mark (entry)
  "设置某一个条目的 * mark"
  nil)

(defun dmp-table-clear-entry-*-mark (entry)
  "清除某一个条目的 * mark"
  nil)

(defun dmp-table-set-entry-D-mark (entry)
  "设置某一个条目的 D mark"
  nil)

(defun dmp-table-clear-entry-D-mark (entry)
  "清除某一个条目的 D mark"
  nil)

(defun dmp-table-set-entry-P-mark (entry)
  "设置某一个条目的 P mark"
  nil)

(defun dmp-table-clear-entry-P-mark (entry)
  "清除某一个条目的 P mark"
  nil)

(defun dmp-table-set-entry-U-mark (entry)
  "设置某一个条目的 U mark"
  nil)

(defun dmp-table-clear-entry-U-mark (entry)
  "清除某一个条目的 U mark"
  nil)

(defun dmp-table-trans-entry-*-D-mark (entry)
  "将某个条目的*=>D"
  nil)
(defun dmp-table-trans-entry-*-P-mark (entry)
  "将某个条目的*=>P"
  nil)


(defun dmp-table-reload()
  "重新从硬盘装载playlist，并刷新界面"
  (dmp-player-stop)
  (dmp-info-cancel-all)
  (when (dmp-playlist-load)
    (dmp-recreate-table)
    (dmp-info-start-batch)
    (tabulated-list-print)))


(defun dmp-table-create-entries (&optional merge)
  "生成dmp-table-entries"
  nil)

(defun dmp-table-open-buffer ()
  "打开DMP的主界面"
  (interactive)
  (dmp-log-debug "dmp-open-buffer called")
  (let* ((buffer-name dmp-buffer-name)
         (buffer (get-buffer buffer-name)))
    (unless buffer
      (setq buffer (generate-new-buffer buffer-name))
      (with-current-buffer buffer
        (dmp-table-mode)
        (dmp-playlist-init)
        (dmp-info-init)
        (dmp-player-init)
        (dmp-table-reload)))))



(defun dmp-recreate-table ()
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
  (setq tabulated-list-entries dmp-table-entries-alist)
  (tabulated-list-init-header))

(provide dmp')
