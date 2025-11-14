;; 阅读词汇

(defcustom mysay-program "~/.emacs.d/plugin.d/spell/mysay" "Localtion of mysay program")

(defun get-process-output (command)
  "Call PROGRAM with ARGS and return its stdout as a string."
  (let ((output-buffer (generate-new-buffer "*Process Output*")))
    (unwind-protect
        (progn
          (call-process "/bin/sh" nil output-buffer nil "-c" command)
          (with-current-buffer output-buffer
            (buffer-string)))
      (kill-buffer output-buffer))))

(defun pronounce-word()
  "Pronounce the word at point."
  (interactive)
  (let ((word (downcase (current-word)))
        (output))
    (message "Will pronounce '%s'...." word)
    (cond 
     ((eq system-type 'darwin)
      (setq output (get-process-output (format "%s '%s'" mysay-program word))))
     (t (user-error (format "Unsupported OS: %s" system-type))))
    (message output)))

(global-set-key (kbd "C-c p") 'pronounce-word)


(provide 'config-spell) 
