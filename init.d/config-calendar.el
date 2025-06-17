;;设置日历

(require 'calendar)

(defconst my-diary-root "/Volumes/Diary" "root directory or diary archive")

(defconst my-diary-skel-string
"## <font color=green>天气：晴</font>\n\
\n\
## <font color=green>%04d.%02d.%02d %s</font>\n\
\n\
------------------------------------------------\n\
\n\
## 大事记 ##\n")

(defun my-diary-create-empty-diary (file year mon day)
  (message "my-diary-create-empty-diary %s" file)
  (switch-to-buffer (find-file file))
  (insert (format my-diary-skel-string
                  year mon day (calendar-day-name current-date nil)))
  (save-buffer))

(defun my-diary-prepare-archive (diary-root)
  (message "diary root is %s" diary-root)
  (unless (file-accessible-directory-p my-diary-root)
    (let ((my-diary-pass))
      (setq my-diary-pass (read-passwd "Enter diary archive pass:"))
      (call-process "/usr/bin/open" nil "*Messages*" nil "-g" (format "smb://michael:%s@share/Diary" my-diary-pass))))
  (let ((cnt 0) (ready nil))
    (while (and ( < cnt 10) (not ready))
      (progn
        (sit-for 1)
        (if (and (file-accessible-directory-p my-diary-root) (file-writable-p my-diary-root))
            (setq ready t))
        (message "wait for %s ready %d" my-diary-root cnt)
        (setq cnt (+ cnt 1))))
    (if (>= cnt 10)
        (signal 'can-not-prepare-archive' t))))


(defun my-diary-open-diary-file (current-date)
  (message "current date is %s" current-date)
  (let ((diary-dir) (diary-file)))
    (progn
      (setq diary-dir (file-name-concat
                       my-diary-root 
                       (format "%04d" (nth 2 current-date))
                       (format "%02d" (nth 0 current-date))
                       (format "%02d" (nth 1 current-date))))
      (setq diary-file (file-name-concat
                        diary-dir "index.md"))
      (message "diary-file is %s" diary-file)
      (unless (file-accessible-directory-p diary-dir)
        (make-directory diary-dir t))
      (if (file-exists-p diary-file)
          (switch-to-buffer (find-file diary-file))
        (my-diary-create-empty-diary diary-file (nth 2 current-date) (nth 0 current-date) (nth 1 current-date)))))

(defun my-diary-view-entries (&optional arg)
  (interactive "p")
  (let ((current-date (calendar-cursor-to-date t)))         
    (message "my-diary-view-entrie! whill look diary %s" current-date)
    (condition-case err
        (progn
          (my-diary-prepare-archive my-diary-root)
          (my-diary-open-diary-file current-date))
      (file-error
       (message "file error! %s" err))
      (can-not-prepare-archive
       (message "prepare archive failed! %s" err)))))

(defun my-calendar-hook ()

;;设置我所在地方的经纬度，calendar里有个功能是日月食的预测，和你的经纬度相联系的。
;; 让emacs能计算日出日落的时间，在 calendar 上用 S 即可看到
(setq calendar-latitude +39.54)
(setq calendar-longitude +116.28)
(setq calendar-location-name "北京")

(setq calendar-day-name-array ["星期日" "星期一" "星期二" "星期三" "星期四" "星期五" "星期六"]
      calendar-day-abbrev-array ["日" "一" "二" "三" "四" "五" "六"]
      calendar-day-header-array ["日" "一" "二" "三" "四" "五" "六"]
      calendar-month-name-array ["一月" "二月" "三月" "四月" "五月"
	                             "六月" "七月" "八月" "九月"
				     "十月" "十一月" "十二月"])


;; 设置阴历显示，在 calendar 上用 pC 显示阴历
(setq calendar-chinese-celestial-stem
  ["甲" "乙" "丙" "丁" "戊" "己" "庚" "辛" "壬" "癸"])
(setq calendar-chinese-terrestrial-branch
  ["子" "丑" "寅" "卯" "辰" "巳" "戊" "未" "申" "酉" "戌" "亥"])

;; 设置 calendar 的显示
(setq calendar-remove-frame-by-deleting t)
(setq calendar-week-start-day 1) ; 设置星期一为每周的第一天
(setq mark-diary-entries-in-calendar t) ; 标记calendar上有diary的日期
(setq mark-holidays-in-calendar nil) ; 为了突出有diary的日期，calendar上不标记节日

;; 去掉不关心的节日，设定自己在意的节日，在 calendar 上用 h 显示节日
(setq holiday-christian-holidays nil)
(setq holiday-hebrew-holidays nil)
(setq holiday-islamic-holidays nil)
(setq holiday-solar-holidays nil)
(setq holiday-general-holidays '((holiday-fixed 1 1 "元旦")
                         (holiday-fixed 2 14 "情人节")
                         (holiday-fixed 3 14 "白色情人节")
                         (holiday-fixed 4 1 "愚人节")
                         (holiday-fixed 5 1 "劳动节")
                         (holiday-float 5 0 2 "母亲节")
                         (holiday-fixed 6 1 "儿童节")
                         (holiday-float 6 0 3 "父亲节")
                         (holiday-fixed 7 1 "建党节")
                         (holiday-fixed 8 1 "建军节")
                         (holiday-fixed 9 10 "教师节")
                         (holiday-fixed 10 1 "国庆节")
                         (holiday-fixed 12 25 "圣诞节")))
(calendar-mark-holidays)
(local-set-key (kbd "d") 'my-diary-view-entries)

(message "my-calendar-hook hook run"))

(add-hook 'calendar-mode-hook 'my-calendar-hook)

;;Calendar模式支持各种方式来更改当前日期
;;（这里的“前”是指还没有到来的那一天，“后”是指已经过去的日子）
;; q 退出calendar模式
;; C-f 让当前日期向前一天
;; C-b 让当前日期向后一天
;; C-n 让当前日期向前一周
;; C-p 让当前日期向后一周
;; M-} 让当前日期向前一个月
;; M-{ 让当前日期向后一个月
;; C-x ] 让当前日期向前一年
;; C-x [ 让当前日期向后一年
;; C-a 移动到当前周的第一天
;; C-e 移动到当前周的最后一天
;; M-a 移动到当前月的第一天
;; M-e 多动到当前月的最后一天
;; M-< 移动到当前年的第一天
;; M-> 移动到当前年的最后一天

;;Calendar模式支持移动多种移动到特珠日期的方式
;; g d 移动到一个特别的日期
;; o 使某个特殊的月分作为中间的月分
;; . 移动到当天的日期
;; p d 显示某一天在一年中的位置，也显示本年度还有多少天。
;; C-c C-l 刷新Calendar窗口

;; Calendar支持生成LATEX代码。
;; t m 按月生成日历
;; t M 按月生成一个美化的日历
;; t d 按当天日期生成一个当天日历
;; t w 1 在一页上生成这个周的日历
;; t w 2 在两页上生成这个周的日历
;; t w 3 生成一个ISO-SYTLE风格的当前周日历
;; t w 4 生成一个从周一开始的当前周日历
;; t y 生成当前年的日历

;;EMACS Calendar支持配置节日：
;; h 显示当前的节日
;; x 定义当天为某个节日
;; u 取消当天已被定义的节日
;; e 显示所有这前后共三个月的节日。
;; M-x holiday 在另外的窗口的显示这前后三个月的节日。


;; 另外，还有一些特殊的，有意思的命令：
;; S 显示当天的日出日落时间(是大写的S)
;; p C 显示农历可以使用
;; g C 使用农历移动日期可以使用


;; (require 'calfw)
;; (require 'calfw-cal)
;; (require 'calfw-ical)
;; ;纪念日.ics       旅行.ics         其它.ics         娃校内.ics       娃校外.ics       学习计划.ics     运动.ics 
;; (defun my-open-calendar ()
;;   (interactive)
;;   (cfw:open-calendar-buffer
;;    :contents-sources
;;    (list
;;     (cfw:ical-create-source "纪念日" "~/.emacs.d/caldav/纪念日.ics" "#ff0000" )
;;     (cfw:ical-create-source "旅行" "~/.emacs.d/caldav/旅行.ics" "#7D18F9" )
;;     (cfw:ical-create-source "娃校内" "~/.emacs.d/caldav/娃校内.ics" "#2DEFF8" )
;;     (cfw:ical-create-source "娃校外" "~/.emacs.d/caldav/娃校外.ics" "#7286DC" )
;;     (cfw:ical-create-source "学习计划" "~/.emacs.d/caldav/学习计划.ics" "#03D74C" )
;;     (cfw:ical-create-source "运动" "~/.emacs.d/caldav/运动.ics" "#FECC00" )
;;     (cfw:ical-create-source "其它" "~/.emacs.d/caldav/其它.ics" "#D07669" )
;;     )))

(provide 'config-calendar)
