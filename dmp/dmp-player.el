;; DMP播放器模块

(defun dmp-player-init ()
  "player初始化"
  nil
  )

(defun dmp-player-set-playlist (list)
  "设置播放列表"
  (dmp-log-debug "set new playlist %s" list))

(defun dmp-player-play (id)
  "播放特定乐曲"
  nil)

(defun dmp-player-play-pause ()
  "播放、暂停当前歌单"
  nil)

(defun dmp-player-stop ()
  "停止播放"
  nil)

(defun dmp-player-toggle-loop-style ()
  "切换循环方式"
  nil)

(defun dmp-player-volume-up ()
  "提高音量"
  nil)

(defun dmp-player-volume-down ()
  "提高音量"
  nil)

(defun dmp-player-track-prev()
  "下一首歌"
  nil)

(defun dmp-player-track-next()
  "上一首歌"
  nil)




(provide 'dmp-player)
