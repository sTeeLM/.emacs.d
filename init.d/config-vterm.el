;; -*- lexical-binding: t; -*-
(require 'vterm)

;; 显示完整路径
(setq vterm-buffer-name-string "vterm %s")

;; 使用bash
(setq vterm-shell "/opt/local/bin/bash")



(defun my-vterm-mode-fun ()
  (turn-off-auto-fill)
  (setq vterm-timer-delay nil)
  (define-key vterm-mode-map (kbd "C-c C-SPC") #'toggle-input-method)
  (message "USE VTERM!"))

(add-hook 'vterm-mode-hook 'my-vterm-mode-fun)

                                        ;
;;(lambda ()
;      ;; C-x is the prefix command, rather than C-c
;      (term-set-escape-char ?\C-x)
;      (define-key term-raw-map "\M-y" 'yank-pop)
;      (define-key term-raw-map "\M-w" 'kill-ring-save)))


(add-to-list 'vterm-eval-cmds '("scratch" scratch-buffer))

(provide 'config-vterm)

