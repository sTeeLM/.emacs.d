;; -*- lexical-binding: t; -*-
;; 中文字体的设置

;; aaaaaaaaaaaaaaaaaaaaaaaaaaaa
;; 这行中文应该和上面的英文对齐

;; 中文字体的自动对齐
;; (require 'cnfonts)
;; 让 cnfonts 在 Emacs 启动时自动生效。
;;(cnfonts-mode 1)
;; 添加两个字号增大缩小的快捷键
;;(define-key cnfonts-mode-map (kbd "C--") #'cnfonts-decrease-fontsize)
;;(define-key cnfonts-mode-map (kbd "C-=") #'cnfonts-increase-fontsize)
;; 显示所有可用的字体
;;(cl-prettyprint (font-family-list))

;; 自定义字体变量
;;(setq cnfonts-personal-fontnames
       ;; '(
       ;;   ;; 英文字体
       ;;   ("Ubuntu Mono")
       ;;   ;; 中文字体
       ;;   ("Maple Mono NF CN")
       ;;   ;; EXT-B
       ;;   ()
       ;;   ;; Symbol 字符字体
       ;;   ()
       ;;   ))



;;;;;;;;;;;;;;;;; 使用自定义的字体函数
(defun my-setup-fonts (frame)
;;   "在指定的 FRAME 激活时，设置中英文字体。"
  (with-selected-frame frame
    (when (display-graphic-p frame) ; 确保只在图形界面（GUI）下设置，不影响终端（TUI）
      ;; 1. 设置英文字体及大小
      (set-face-attribute 'default frame :font "Ubuntu Mono 16")
;;       ;; 设置eww的字体
      (set-face-attribute 'variable-pitch nil  :font "Ubuntu Mono 16"   ; 替换为你系统中安装的字体名和字号
                          :weight 'normal)
;;       ;; 2. 设置中文字体
      (dolist (charset '(han symbol cjk-misc))
        (set-fontset-font t charset (font-spec :family "Maple Mono NF CN" :size 20) frame)))))

;; 核心：将函数挂载到 Frame 创建的钩子上
(add-hook 'after-make-frame-functions #'my-setup-fonts)

;; 触发机制：确保启动时的第一个主 Frame 也能正确加载字体
(my-setup-fonts (selected-frame))

;; 1. 手动：设置英文字体及大小
;;(set-face-attribute 'default nil :font "Ubuntu Mono 16")

;; 2. 手动：设置中文字体
;;(dolist (charset '(han symbol cjk-misc))
;;  (set-fontset-font t charset (font-spec :family "Maple Mono NF CN" :size 20)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 浏览器默认用等宽字体，在EWW下F切换字体
(setq shr-use-fonts nil)

(provide 'config-font)
