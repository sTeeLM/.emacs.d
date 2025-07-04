;; Mark Down配置

;; 添加md模式
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; 使用w3m预览
(defun markdown-live-preview-window-w3m (file)
  "Preview FILE with w3m. To be used with `markdown-live-preview-window-function'."
  (if (require 'w3m nil t)
      (let ((w3m-display-mode 'plain))
        (w3m (concat "file://" file) t)
        (get-buffer "*w3m*"))
    (error "w3m is not present or not loaded on this version of Emacs")))

;; 使用lynx预览，C-l/C-w刷新页面
(defun markdown-live-preview-window-lynx (file)
  "Preview FILE with lynx. To be used with `markdown-live-preview-window-function'."
  (term-run-command (format "lynx %s" file) "lynx" t))

;; 在这行选择用什么方式预览
;; markdown-live-preview-window-lynx
;; markdown-live-preview-window-w3m
;; 注释掉使用默认eww
; (setq markdown-live-preview-window-function 'markdown-live-preview-window-lynx)


;; 将一个文件copy到同级assets目录下，并插入markdown文档中
;; item: 文件路径,相对\绝对都可以
;; is-image: 是一个图片吗？
;; with-link: 如果是图片，需要带点击链接吗？
;; 注意：非图片文件插入文档，总是可以点击
(defun markdown-insert-inline-item-from-file (item is-image &optional with-link)
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
                (if with-link
                 (markdown-insert-inline-link
                  (format "![%s](%s)" (file-name-nondirectory item) (url-encode-url assets-item-url))
                  (url-encode-url assets-item-url)
                  (file-name-nondirectory item))
                 (markdown-insert-inline-image
                  (file-name-nondirectory item)
                  (url-encode-url assets-item-url)
                  (file-name-nondirectory item)))
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

;; 插入内嵌图片到文件中，该文件将被copy到assets目录
;; 并且带有点击链接
(defun markdown-insert-clickable-inline-image-from-file (image)
  "copy file to 'assets', insert it as inline image, with clickable link"
  (interactive "fInsert image file: ")
  (markdown-insert-inline-item-from-file image t t))


;; 插入内嵌图片到文件中，该文件将被copy到assets目录
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
;; Bold字体使用红色
  (set-face-attribute 'markdown-bold-face nil :foreground "brightcyan")
  (set-face-attribute 'bold nil :foreground "brightcyan")
  (auto-fill-mode -1)
  (local-set-key (kbd "C-c C-d") 'insert-current-date)
  (local-set-key (kbd "C-c i") 'markdown-insert-inline-image-from-file)
  (local-set-key (kbd "C-c I") 'markdown-insert-clickable-inline-image-from-file)
  (local-set-key (kbd "C-c f") 'markdown-insert-inline-link-from-file))

(add-hook 'markdown-mode-hook 'my-md-hook)

;; Markdown可以预览
;(custom-set-variables
(setq markdown-command "pandoc -s -c /Users/michael/.emacs.d/pandoc/pandoc.css --metadata title=preview")


(provide 'config-markdown)
