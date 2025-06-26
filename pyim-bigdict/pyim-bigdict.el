;; 注意：这个词库的词条量大概在 100 万左右，是一个 *比较大* 的词库，只能确保 pyim
;; 可以正常工作，如果用户想让 pyim 更加顺手，需要添加其它附加词库，具体添加词库的
;; 方式可以参考 pyim 的 README.

;; ** 安装和使用
;; 1. M-x package-install RET pyim-bigdict RET
;; 2. 在 Emacs 配置文件中（比如: "~/.emacs"）添加如下代码：
;;    #+BEGIN_EXAMPLE
;;    (pyim-bigdict-enable)
;;    #+END_EXAMPLE

;;; Code:
;; * 代码                                                               :code:
(require 'pyim-dict)

;;;###autoload
(defun pyim-bigdict-enable ()
  "Add pyim-bigdict.pyim file to `pyim-extra-dicts'."
  (interactive)
  (let ((file (expand-file-name
               "pyim-bigdict.pyim"
               (file-name-directory
                (locate-library "pyim-bigdict.el")))))
    (when (file-exists-p file)
      (pyim-extra-dicts-add-dict
       `(;; Make Indent beautiful :-)
         :name "Bigdict-elpa"
         :file ,file
         :coding utf-8-unix
         :dict-type pinyin-dict
         :elpa t)))))

;; * Footer
(provide 'pyim-bigdict)

;;; pyim-bigdict.el ends here
