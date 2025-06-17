(require 'vterm)

;; 显示完整路径
(setq vterm-buffer-name-string "vterm %s")

;; 使用bash
(setq vterm-shell "/bin/bash")

(add-hook 'vterm-mode-hook
 (lambda ()
  (auto-fill-mode -1)
  (message "USE VTERM!")))
; (lambda ()
;      ;; C-x is the prefix command, rather than C-c
;      (term-set-escape-char ?\C-x)
;      (define-key term-raw-map "\M-y" 'yank-pop)
;      (define-key term-raw-map "\M-w" 'kill-ring-save)))


(defun create-scratch-buffer nil
  "create a scratch buffer and switch to it"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))
 
(add-to-list 'vterm-eval-cmds '("scratch" create-scratch-buffer))

(provide 'config-vterm)

