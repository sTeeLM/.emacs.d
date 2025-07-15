;;公用的库

;;;;;;;;;;;;;;;;;日志库
(defvar sm-log-buffer-name  "*sTeeL Log List*" "默认的日志buffer名称")

(defvar sm-log-max-level 3 "min log level: debug 3, info 2, warn 1, error 0")

(defface sm-log-debug-face
  '((t (:foreground "#ffffff")))
  "debug log face")

(defface sm-log-info-face
  '((t (:foreground "#00ff00")))
  "info log face")

(defface sm-log-warn-face
  '((t (:foreground "#ffff00")))
  "warn log face")

(defface sm-log-error-face
  '((t (:foreground "#ff0000")))
  "error log face")


(define-derived-mode sm-log-mode fundamental-mode "sTeeL Log List" "日志buffer的major mode")

(defun sm-log-no-echo (level local-level name msg &optional buffer-name)
  (when (and (<= level local-level) (<= level sm-log-max-level))
    (let ((buffer (get-buffer (or buffer-name sm-log-buffer-name))))
      (unless buffer
        (with-current-buffer (get-buffer-create (or buffer-name sm-log-buffer-name))
          (setq buffer (current-buffer))
          (sm-log-mode)))
      (with-current-buffer buffer
        (goto-char (point-max))
        (setq buffer-read-only nil)
        (insert
         (cond
          ((= level 3) (propertize (format "[DBG ][%s] %s\n" name msg) 'face 'sm-log-debug-face))
          ((= level 2) (propertize (format "[INFO][%s] %s\n" name msg) 'face 'sm-log-info-face))
          ((= level 1) (propertize (format "[WARN][%s] %s\n" name msg) 'face 'sm-log-warn-face))
          ((= level 0) (propertize (format "[ERR ][%s] %s\n" name msg) 'face 'sm-log-error-face))
          (t (format "[????][%s] %s" name msg))))
        (setq buffer-read-only t)))))


(defmacro sm-log-make-logger-level (name)
  `(defvar ,(intern (format "%s-log-max-level" name)) 3))

(defmacro sm-log-make-logger-debug (name &optional buffer-name)
  `(defun ,(intern (format "%s-log-debug" name)) (fmt &rest args)
     (let ((msg (apply 'format fmt args)))
       (sm-log-no-echo 3 ,(intern (format "%s-log-max-level" name)) ,name msg ,buffer-name))))

(defmacro sm-log-make-logger-info (name &optional buffer-name)
  `(defun ,(intern (format "%s-log-info" name)) (fmt &rest args)
     (let ((msg (apply 'format fmt args)))
       (sm-log-no-echo 2 ,(intern (format "%s-log-max-level" name)) ,name msg ,buffer-name))))

(defmacro sm-log-make-logger-warn (name &optional buffer-name)
  `(defun ,(intern (format "%s-log-warn" name)) (fmt &rest args)
     (let ((msg (apply 'format fmt args)))
       (sm-log-no-echo 1 ,(intern (format "%s-log-max-level" name)) ,name msg ,buffer-name))))

(defmacro sm-log-make-logger-error (name &optional buffer-name)
  `(defun ,(intern (format "%s-log-error" name)) (fmt &rest args)
     (let ((msg (apply 'format fmt args)))
       (sm-log-no-echo 0 ,(intern (format "%s-log-max-level" name)) ,name msg ,buffer-name))))

(provide 'config-lib)
