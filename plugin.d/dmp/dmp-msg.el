;; DMP 日志模块

(require 'config-lib)

(sm-log-make-logger-level "dmp")
(sm-log-make-logger-debug "dmp" "*DMP Messages*")
(sm-log-make-logger-info  "dmp" "*DMP Messages*")
(sm-log-make-logger-warn  "dmp" "*DMP Messages*")
(sm-log-make-logger-error "dmp" "*DMP Messages*")

(provide 'dmp-msg)
