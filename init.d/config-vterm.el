(require 'vterm)

;; 显示完整路径
(setq vterm-buffer-name-string "vterm %s")

;; 使用bash
(setq vterm-shell "/bin/bash")


(defun my-vter-mode-fun ()
  (turn-off-auto-fill)
  (setq vterm-timer-delay 0.01)
  ;;  (term-set-escape-char ?\C-x)
  ;; (define-key term-raw-map "\M-y" 'yank-pop)
  ;; (define-key term-raw-map "\M-w" 'kill-ring-save)
  (message "USE VTERM!"))

(add-hook 'vterm-mode-hook 'my-vter-mode-fun)

                                        ;
;;(lambda ()
;      ;; C-x is the prefix command, rather than C-c
;      (term-set-escape-char ?\C-x)
;      (define-key term-raw-map "\M-y" 'yank-pop)
;      (define-key term-raw-map "\M-w" 'kill-ring-save)))


(add-to-list 'vterm-eval-cmds '("scratch" scratch-buffer))

(provide 'config-vterm)

