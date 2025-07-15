;;音乐播放
(require 'emms-setup)
(emms-all)

;; 默认播放器，信息解析器的设置
(setq emms-player-list '(emms-player-mplayer))
(setq emms-info-functions '(emms-info-native))

;; 默认buffer名称
(setq emms-playlist-buffer-name "*Music*")

;; history
;;(emms-history-load)

;; 异步装载info
(setq emms-info-asynchronously t) 

(setq emms-source-playlist-default-format 'm3u)

(setq emms-playlist-default-major-mode 'emms-mark-mode)

(setq emms-show-format "NP: %s")


(defun emm ()
  "播放音乐"
  (interactive)
  (emms)
  (emms-playlist-clear)
  (emms-add-playlist "~/.emacs.d/emms.m3u"))

(provide 'music)
