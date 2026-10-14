;; -*- lexical-binding: t; -*-
;; DMP 库函数

(defvar dmp-lisp-max-length 20000000
  "默认元数据最大长度")

(defmacro dmp-frwlet (read write &rest body)
  ;; (declare (indent 2))
  `(let ((coding-system-for-read  ,read)
	     (coding-system-for-write ,write)
	     (format-alist nil)
	     (auto-image-file-mode nil)
	     (jka-compr-inhibit t))
     ,@body))
(put 'dmp-frwlet 'lisp-indent-function 2)

(defmacro dmp-ntake (n lst)
  `(if (> (length ,lst) ,n) (setcdr (nthcdr (1- ,n) ,lst) nil)))

(defun dmp-lisp-load (filename)
  "Load lisp from FILENAME"
  (let ((fullname (expand-file-name filename)))
    (if (file-readable-p fullname)
	    (with-temp-buffer
	      (dmp-frwlet 'utf-8-unix 'utf-8-unix
	        (insert-file-contents fullname))
	      (goto-char (point-min))
	      (condition-case nil
	          (read (current-buffer))
	        (error ()))))))

(defun dmp-lisp-save (filename lisp &optional nobackup unlimit)
  "Save LISP to FILENAME. LISP is truncated to dmp-lisp-max-length
by side-effect."
  (let* ((fullname (expand-file-name filename mew-conf-path))
	     (backname (concat fullname ".BAK"))
	     print-length print-level) ;; for Emacs 21
    (when (file-writable-p fullname)
      (if nobackup
	      (delete-file fullname)
	    (if (file-exists-p fullname)
	        (rename-file fullname backname 'override)))
      (when (and (not unlimit) (> (length lisp) dmp-lisp-max-length))
	    (setq lisp (copy-sequence lisp)) ;; no side effect
	    (dmp-ntake mew-lisp-max-length lisp))
      (with-temp-buffer
	    (if (> (length lisp) mew-lisp-max-length)
	        (print lisp (current-buffer))
	      (pp lisp (current-buffer)))
	    (dmp-frwlet 'utf-8-unix 'utf-8-unix
	      (write-region (point-min) (point-max) fullname nil 'no-msg))))))

(provide 'dmp-lisp)
