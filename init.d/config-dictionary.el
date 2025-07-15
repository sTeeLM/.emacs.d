;; 词典配置

(require 'dictionary)

;; 查找本地服务器
(setq dictionary-server "server.home.madcat.cc")

;; C-c l 查找单词
(global-set-key (kbd "C-c l") 'dictionary-lookup-definition)

;; 默认查找中文词典
(setq dictionary-default-dictionary "*")

;; ipsell设置
(setq ispell-program-name "ispell")

;; 阅读词汇
(defun pronounce-word-or-region()
  "Pronounce the word at point."
  (interactive)
  (let ((word (current-word))
        (region
         (when (use-region-p)
           (buffer-substring-no-properties (region-beginning) (region-end))))
        (text))
    (if region
        (setq text region)
      (setq text word))
    (cond 
     ((eq system-type 'darwin)  (call-process-shell-command (format "say %s" text)))
     (t (user-error (format "Unsupported OS: %s" system-type)))) ))

(global-set-key (kbd "C-c p") 'pronounce-word-or-region)


(provide 'config-dictionary) 
