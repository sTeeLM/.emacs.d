;; DMP playlist元数据和相关管理函数

;; 元数据格式
;; 以媒体文件的绝对路径为key的alist
;; (("abs-file-path" . [xxx]) .. .. )
;; P     : in the player's job list
;; I     : info 信息有效
;; type  : 文件类型 MP3 WAV OGG
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
  "对核心元数据的锁"
  )

(defun dmp-playlist-init()
  "初始化"
  (setq dmp-playlist-mutex (make-mutex "dmp-playlist-mutex")))

(defun dmp-playlist-load ()
  "从硬盘装载playlist元数据"
  nil)

(defun dmp-playlist-save ()
  "保存playlist元数据到硬盘"
  nil)

(defun dmp-playlist-append-filelist (filelist)
  "向列表中追加playlist"
  nil)

(defun dmp-playlist-append-file (file)
  "向列表中追加单个文件"
  nil)

(defun dmp-playlist-append-directory (directory)
  "向列表中追加整个目录"
  nil)

(defun dmp-playlist-remove-entries (entries)
  "删除列表"
  nil)


(provide 'dmp-playlist)
