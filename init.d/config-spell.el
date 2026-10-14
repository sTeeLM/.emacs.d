;; -*- lexical-binding: t; -*-
;; 阅读词汇

(defcustom mysay-program "~/.emacs.d/plugin.d/spell/mysay" "Localtion of mysay program")
(defcustom say-program "/usr/bin/say" "Localtion of say program")

(defun get-process-output (command)
  "Call PROGRAM with ARGS and return its stdout as a string."
  (let ((output-buffer (generate-new-buffer "*Process Output*"))
        (output-string "")
        (retval 0))
    (unwind-protect
        (progn
          (setq retval (call-process "/bin/sh" nil output-buffer nil "-c" command))
          (with-current-buffer output-buffer
            (setq output-string (buffer-string))))
      (kill-buffer output-buffer))
    (list retval output-string)))

(defun pronounce-word()
  "Pronounce the word at point."
  (interactive)
  (let ((word (downcase (current-word)))
        (output))
    (message "Will pronounce '%s'...." word)
    (cond 
     ((eq system-type 'darwin)
      (progn
        (setq output (get-process-output (format "python3 -W ignore::UserWarning %s '%s'" mysay-program word)))
        (if (= 0 (car output))
            (message (cadr output))
          (progn
            (message  (format "Called second approach to pronounce '%s'" word))
            (call-process "/bin/sh" nil nil nil "-c"  (format "%s '%s'" say-program word))))))
     (t (user-error (format "Unsupported OS: %s" system-type))))))

(global-set-key (kbd "C-c p") 'pronounce-word)


(provide 'config-spell) 


