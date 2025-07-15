;; dummy 音乐播放器
;; 以一个m3u格式的文件为媒体库的
;; 及其简单的表格界面的音乐播放器

(require 'dmp-msg)
(require 'dmp-player)
(require 'dmp-playlist')
(require 'dmp-info)

(defconst dmp-playlist-buffer-name "*Music*")

(defvar dmp-playlist-entries  nil
  "播放列表界面数据")

(defun dmp-buffers-revert () nil)

(define-derived-mode dmp-playlist-mode tabulated-list-mode "DMP Music List"
  "DMP playlist interface major mode"
  (add-hook 'tabulated-list-revert-hook 'dmp-buffers-revert nil t)
  nil)

(define-key dmp-playlist-mode-map  "p"  'dmp-play-pause)
(define-key dmp-playlist-mode-map  "s"  'dmp-stop)
(define-key dmp-playlist-mode-map  "t"  'dmp-toggle-loop-style)
(define-key dmp-playlist-mode-map  "+"  'dmp-volume-up)
(define-key dmp-playlist-mode-map  "-"  'dmp-volume-down)
(define-key dmp-playlist-mode-map  ">"  'dmp-track-next)
(define-key dmp-playlist-mode-map  "<"  'dmp-track-prev)
(define-key dmp-playlist-mode-map  (kbd "RET") 'dmp-test-current)
(define-key dmp-playlist-mode-map  "*"  'dmp-mark)
(define-key dmp-playlist-mode-map  "u"  'dmp-unmark)
(define-key dmp-playlist-mode-map  "U"  'dmp-unmark-all)
(define-key dmp-playlist-mode-map  "md" 'dmp-mark-to-delete)
(define-key dmp-playlist-mode-map  "d"  'dmp-mark-delete)
(define-key dmp-playlist-mode-map  "x"  'dmp-excute-delete)
(define-key dmp-playlist-mode-map  "mp" 'dmp-mark-to-add-playlist)
(define-key dmp-playlist-mode-map  "mu" 'dmp-mark-to-remove-playlist)
(define-key dmp-playlist-mode-map  "g"  'dmp-reload)
(define-key dmp-playlist-mode-map  "C-x G" 'dmp-rebuild-all-info)
(define-key dmp-playlist-mode-map  "G"  'dmp-rebuild-info)
(define-key dmp-playlist-mode-map  "al"  'dmp-add-list)
(define-key dmp-playlist-mode-map  "af"  'dmp-add-file)
(define-key dmp-playlist-mode-map  "C-x S" 'dmp-save)

(defun dmp-save ()
  "保存播放列表"
  (interactive)
  (dmp-playlist-save))

(defun dmp-reload()
  "重新从硬盘装载playlist"
  (interactive)
  (dmp-playlist-load)
  (dmp-playlist-export-entries)
  (dmp-info-start-batch)
  (dmp-recreate-table)
  (tabulated-list-print))

(defun dmp-rebuild-all-info ()
  "重新更新一遍所有元数据"
  (interactive)
  (dmp-info-start-batch t)
  nil)

(defun dmp-rebuild-info ()
  "重新更新所在条目元数据"
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (dmp-info-start-oneshot id)))


(defun dmp-add-list(file-list)
  "从文件列表添加歌曲到playlist"
  (interactive "fSelect List File: ")
  (dmp-playlist-append-file-list (expand-file-name file-list))
  (dmp-playlist-export-entries)
  (dmp-info-start-batch)
  (tabulated-list-print))

(defun dmp-add-file(file)
  "从单个文件添加歌曲到playlist"
  (interactive "fSelect Media File: ")
  (dmp-playlist-append-file (expand-file-name file))
  (dmp-playlist-export-entries)
  (dmp-info-start-batch)
  (tabulated-list-print))

(defun dmp-add-directory(directory)
  "从目录添加歌曲到playlist"
  (interactive "DSelect Directory: ")
  (dmp-playlist-append-directory (expand-file-name directory))
  (dmp-playlist-export-entries)
  (dmp-info-start-batch)
  (tabulated-list-print))

(defun dmp-play-pause ()
  "播放/暂停当前歌单(P条目)"
  (dmp-player-play-pause))

(defun dmp-stop ()
  "停止播放当前曲目/歌单"
  (interactive) 
  (dmp-player-stop))

(defun dmp-test-current ()
  "播放当前选中歌曲，但是不加入歌单"
  (interactive) 
  (dmp-player-play-pause (tabulated-list-get-id)))

(defun dmp-toggle-loop-style ()
  "切换播放方式：单次顺序/单次随机/循环顺序/循环随机"
  (interactive) 
  (dmp-player-toggle-loop-style))

(defun dmp-volume-up()
  "声音增大"
  (interactive)
  (dmp-player-volume-up)) 

(defun dmp-volume-up()
  "声音减少"
  (interactive)
  (dmp-player-volume-down))

(defun dmp-track-next ()
  "下一首歌曲"
  (interactive)
  (dmp-player-track-next))

(defun dmp-track-prev ()
  "上一首歌曲"
  (interactive)
  (dmp-player-track-prev))

(defun dmp-mark ()
  "设置标记"
  nil)

(defun dmp-unmark ()
  "清除标记 * D"
  nil)

(defun dmp-unmark-all ()
  "清除所有标记 * D"
  nil)

(defun dmp-mark-to-delete ()
  "* => D"
  nil)

(defun dmp-mark-delete()
  " => D"
  )

(defun dmp-mark-to-add-playlist ()
  "* => P"
  nil)

(defun dmp-mark-to-remove-playlist ()
  "* => "
  nil)

(defun dmp-excute-delete ()
  "D => delete"
  nil)

(defun dmp-open-buffer ()
  "打开DMP的主界面"
  (interactive)
  (dmp-log-debug "dmp-open-buffer called")
  (let* ((buffer-name (dmp-playlist-buffer-name))
         (buffer (get-buffer buffer-name)))
    (unless buffer
      (setq buffer (generate-new-buffer buffer-name))
      (with-current-buffer buffer
        (dmp-playlist-mode)
        (dmp-reload)))))

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
         ;; genre
         '("GENRE" 12 t :left-align t)
         ;; TIME
         '("TIME" 12 t :left-align t)
         ))
  (setq tabulated-list-use-header-line t)
  (setq tabulated-list-entries dmp-playlist-entries)
  (tabulated-list-init-header))

(provide dmp')
