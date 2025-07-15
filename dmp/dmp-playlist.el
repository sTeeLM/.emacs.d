


;; 元数据格式
;; (("abs-file-path" .  [to-play has-info playing updateing
;;                       'type "Title" "Author" "Album" "Track" "Genre" ])...)
(defvar dmp-playlist-alist  nil
  "核心元数据，整个播放列表")


(defun dmp-playlist-load ()
  "装载playlist元数据"
  nil)

(defun dmp-playlist-export-entries ()
  "从核心元数据产生界面元数据"
  nil)

(defun dmp-playlist-append-file-list (file-list)
  "向列表中追加playlist"
  nil)

(defun dmp-playlist-append-file (file)
  "向列表中追加单个文件"
  nil)

(defun dmp-playlist-append-directory (dir)
  "向列表中追加整个目录"
  nil)



(provide 'dmp-playlist)
