;; Mark Down配置

;; 添加md模式
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; 插入图片到文件中，该文件将被copy到assets目录
(defun markdown-insert-inline-image-from-file (image)
  "copy file to 'assets', nsert it as inline image"
  (interactive "FInsert image file: ")
  (let
      (
       (image-file-path) ; src image file abs path
       (assets-path)  ; assets directory abs path
       (assets-image-path) ; assets image file abs path (dest)
       (assets-image-url)) ; assets image url
      (condition-case err
          (progn
            (setq image-file-path (expand-file-name image))
            (setq assets-path (file-name-concat (file-name-directory (buffer-file-name)) "assets"))
            (setq assets-image-path (file-name-concat assets-path (file-name-nondirectory image)))
            (setq assets-image-url (file-name-concat "assets" (file-name-nondirectory image)))
;            (message "image-file-path is %s" image-file-path)
;            (message "assets-path is %s" assets-path)
;            (message "assets-image-path is %s" assets-image-path)
;            (message "assets-image-url is %s" (url-encode-url assets-image-url))
            (unless (file-exists-p assets-path)
              (make-directory assets-path))
            (copy-file
             image-file-path
             assets-image-path 0) ; if exist, prompt overwirte?
            (markdown-insert-inline-image
             (file-name-nondirectory image)
             (url-encode-url assets-image-url)
             (file-name-nondirectory image))
            )
        (file-already-exists
         (message "Destination file already exists." err))
        (file-missing
         (message "File missing %s" err))
        )
      )
  )


;; 插入当前日期时间，例如：Wed, 31 Aug 2022 11:58:05 +0800
(defun insert-current-date ()
  "Insert the current date"
  (interactive "*")
  (shell-command "date -R" (current-buffer)))

(defun my-md-hook ()
  (message "run my-md-hook")
  (markdown-live-preview-mode)
  (auto-fill-mode -1)
  (local-set-key (kbd "C-c C-d") 'insert-current-date)
  (local-set-key (kbd "C-c i") 'markdown-insert-inline-image-from-file))

(add-hook 'markdown-mode-hook 'my-md-hook)

;; Markdown可以预览
(custom-set-variables
 '(markdown-command "pandoc"))

(provide 'config-markdown)
