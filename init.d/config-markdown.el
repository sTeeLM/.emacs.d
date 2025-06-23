;; Mark Down配置

;; 添加md模式
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


(defun markdown-insert-inline-item-from-file (item is-image)
  "copy file to 'assets', insert it as inline image or link"
  (let
      (
       (item-file-path) ; src file abs path
       (assets-path)  ; assets directory abs path
       (assets-item-path) ; assets item file abs path (dest)
       (assets-item-url)) ; assets item url
      (condition-case err
          (progn
            (setq item-file-path (expand-file-name item))
            (setq assets-path (file-name-concat (file-name-directory (buffer-file-name)) "assets"))
            (setq assets-item-path (file-name-concat assets-path (file-name-nondirectory item)))
            (setq assets-item-url (file-name-concat "assets" (file-name-nondirectory item)))
;            (edebug-trace "item-file-path is %s" item-file-path)
;            (edebug-trace "assets-path is %s" assets-path)
;            (edebug-trace "assets-item-path is %s" assets-item-path)
;            (edebug-trace "assets-item-url is %s" (url-encode-url assets-item-url))
            (unless (file-exists-p assets-path)
              (make-directory assets-path))
            (copy-file
             item-file-path
             assets-item-path 0) ; if exist, prompt overwirte?
            (if is-image
                (markdown-insert-inline-image
                 (file-name-nondirectory item)
                 (url-encode-url assets-item-url)
                 (file-name-nondirectory item))
              (markdown-insert-inline-link
               (file-name-nondirectory item)
               (url-encode-url assets-item-url)
               (file-name-nondirectory item))))
        (file-already-exists
         (message "Destination file already exists." err))
        (file-missing
         (message "File missing %s" err))
        )
      )  
  )

;; 插入图片到文件中，该文件将被copy到assets目录
(defun markdown-insert-inline-image-from-file (image)
  "copy file to 'assets', insert it as inline image"
  (interactive "fInsert image file: ")
  (markdown-insert-inline-item-from-file image t))

;; 插入图片到文件中，该文件将被copy到assets目录
(defun markdown-insert-inline-link-from-file (image)
  "copy file to 'assets', insert it as inline link"
  (interactive "fInsert file: ")
  (markdown-insert-inline-item-from-file image nil))


;; 插入当前日期时间，例如：Wed, 31 Aug 2022 11:58:05 +0800
(defun insert-current-date ()
  "Insert the current date"
  (interactive "*")
  (shell-command "date -R" (current-buffer)))

(defun my-md-hook ()
  (message "run my-md-hook")
  (markdown-live-preview-mode)
;  (markdown-view-mode)
  (auto-fill-mode -1)
  (local-set-key (kbd "C-c C-d") 'insert-current-date)
  (local-set-key (kbd "C-c i") 'markdown-insert-inline-image-from-file)
  (local-set-key (kbd "C-c f") 'markdown-insert-inline-link-from-file))

(add-hook 'markdown-mode-hook 'my-md-hook)

;; Markdown可以预览
;(custom-set-variables
(setq markdown-command "pandoc -s -c /Users/michael/.emacs.d/pandoc/pandoc.css --metadata title=preview")

(provide 'config-markdown)
