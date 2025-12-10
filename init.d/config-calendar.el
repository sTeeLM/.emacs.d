;;设置日历
(require 'calfw)
(require 'calfw-cal)
(require 'calendar)
(require 'icalendar)
(require 'cl)

;; 在calendar上，按下d，直接显示markdown版本的日记

(defcustom my-diary-root "/Volumes/Diary" "root directory or diary archive")
(defcustom my-diary-skel-string
"<div class=\"weather\">天气：晴</div>\n\
<div class=\"date\">%04d.%02d.%02d %s</div>\n\
------------------------------------------------\n\
\n\
## 1. 日常思考\n\
  * 无\n\
\n\
## 2. 读书\n\
  * 无\n\
\n\
## 3. 大事记\n\
  * 无\n\
\n\
## 4. 日常新闻\n\
\n\
### 1. WSJ\n\
\n\
### 2. BBC\n\
\n\
### 3. Bloomberg\n\
\n\
### 4. Reuters\n\
\n"

"skel content in newly created diary")

(define-error 'prepare-archive-error "can not prepare diary archive")


(defun my-diary-create-empty-diary (file year mon day)
  (message "my-diary-create-empty-diary %s" file)
  (let ((display-buffer-overriding-action
         '(display-buffer-pop-up-window)))
    (display-buffer (find-file file)))
  ;(switch-to-buffer-other-window (find-file file))
  (insert (format my-diary-skel-string
                  year mon day (calendar-day-name current-date nil)))
  (save-buffer))

(defun my-diary-prepare-archive (diary-root)
  (message "diary root is %s" diary-root)
  (unless (file-accessible-directory-p my-diary-root)
    (let ((my-diary-pass))
      (setq my-diary-pass (read-passwd "Enter diary archive pass:"))
      (call-process "/usr/bin/open" nil "*Cal-Messages*" nil "-g" (format "smb://michael:%s@share/Diary" my-diary-pass))))
  (let ((cnt 0) (ready nil))
    (while (and ( < cnt 10) (not ready))
      (progn
        (sit-for 1)
        (if (and (file-accessible-directory-p my-diary-root) (file-writable-p my-diary-root))
            (setq ready t))
        (message "wait for %s ready %d" my-diary-root cnt)
        (setq cnt (+ cnt 1))))
    (if (>= cnt 10)
        (signal 'prepare-archive-error t))))


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
          (let ((display-buffer-overriding-action
                 '(display-buffer-pop-up-window)))
            (display-buffer (find-file diary-file)))
          ;(switch-to-buffer-other-window (find-file diary-file))
        (my-diary-create-empty-diary diary-file (nth 2 current-date) (nth 0 current-date) (nth 1 current-date)))))

(defun my-diary-view-entry (current-date)
  (message "my-diary-view-entrie! whill look diary %s" current-date)
  (condition-case err
      (progn
        (my-diary-prepare-archive my-diary-root)
        (my-diary-open-diary-file current-date))
    (file-error
     (message "file error! %s" err))
    (prepare-archive-error
     (message "prepare archive failed! %s" err))))


(defun cal-view-entries (&optional arg)
  (interactive "p")
  (let ((current-date (calendar-cursor-to-date t)))         
    (my-diary-view-entry current-date)))

;;设置我所在地方的经纬度，calendar里有个功能是日月食的预测，和你的经纬度相联系的。
;; 让emacs能计算日出日落的时间，在 calendar 上用 S 即可看到
(setq calendar-latitude +39.54)
(setq calendar-longitude +116.28)
(setq calendar-location-name "北京")
;; 禁止DST
(setq calendar-daylight-savings-starts nil)
(setq calendar-daylight-savings-ends nil)


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

(setq calendar-holidays holiday-general-holidays)


(defun my-calendar-hook()
  (calendar-mark-holidays)
  (local-set-key (kbd "d") 'cal-view-entries)
  (message "my-calendar-hook hook run"))

(add-hook 'calendar-mode-hook 'my-calendar-hook)


;; 为calfw添加一个外挂，原有的calfw-ical不能支持循环日历
;; 所以，我们添加一部分代码，使用calfw-cal数据源，在此基础
;; 上，将ics文件转化为diary文件，让calfw-cal支持多数据源
;; 非常丑陋的改写。。

(defcustom calfw-cal-ical-url-cache-base "~/.emacs.d/caldav-cache" "base dir of cache files")
(defcustom calfw-cal-warn-prefix "CAL" "prefix of logging")
(define-error 'calfw-download-error "URL download failed")

(defun calfw-cal-test-ics-cache (dfile ics)
  (not (and (file-readable-p dfile) (file-readable-p ics)  (file-newer-than-file-p dfile ics))))


(defun calfw-cal-download-url (url auth-user ics)
  (let ((passwd) (ret 0))
    (if auth-user
        (when (setq passwd (read-passwd (format "Please input password for %s@%s: " auth-user ics)))
          (setq ret (call-process "curl" nil "*Cal-Messages*" nil url "--compressed" "-u" (format "%s:%s" auth-user passwd) "-o" (expand-file-name ics))))
      (setq ret (call-process "curl" nil "*Cal-Messages*" nil url "--compressed" "-o" (expand-file-name ics))))
    (message "curl ret %d" ret)
    (unless (equal ret 0)
      (signal 'calfw-download-error ret))))

(defun calfw-cal-filter-daylight (buffer)
  "remove BEGIN:DAYLIGHT xx END:DAYLIGHT out of buffer，\
因为icalendar-import-buffer处理\
daylight好像有bug"
  (let ((begin nil) (end nil))
    (set-buffer buffer)
    (goto-char 1)
    (setq begin (re-search-forward "BEGIN:DAYLIGHT" nil t))
    (when begin
      (setq end (re-search-forward "END:DAYLIGHT" nil t)))
    (when (and begin end (> begin 16))
      (delete-region (- begin 15) end)
      (message "filter out BEGIN:DAYLIGHT & END:DAYLIGHT from %s!!!" buffer)
      (save-buffer))))

(defun calfw-cal-translate-ics (ics dfile)
  (save-current-buffer
      ;; now load and convert from the ical file
      (let ((buffer))
        (delete-file dfile)
        ; create empty diary file
        (save-buffer (find-file dfile))
        (set-buffer (find-file ics))
        ;; filter BEGIN:DAYLIGHT xx END:DAYLIGHT OUT, some bug here?
        (calfw-cal-filter-daylight (current-buffer))
        (icalendar-import-buffer dfile t nil)
        (setq buffer (get-file-buffer ics))
        (when buffer (kill-buffer buffer))
        (setq buffer (get-file-buffer dfile))
        (when (kill-buffer buffer))
      )))

(defun calfw-cal-to-calendar (name url auth-user begin end)
  (let ((diary-file (file-name-concat calfw-cal-ical-url-cache-base (format "%s.%s" name "diary")))
        (ics-file (file-name-concat calfw-cal-ical-url-cache-base (format "%s.%s" name "ics"))))
    ;; if diary-file not exist, or older than ics, or ics not exist, read from url
    (when (calfw-cal-test-ics-cache diary-file ics-file)
      (condition-case err
          (progn
            (calfw-cal-download-url url auth-user ics-file)
            (calfw-cal-translate-ics ics-file diary-file))
        (file-error
         (message "file error %s" err))
        (calfw-download-error
         (message "url download error %s" (cdr err)))))
      (if (file-readable-p diary-file)
          (calfw-cal--schedule-period-to-calendar begin end)
        (display-warning calfw-cal-warn-prefix (format "diary file %s not exist or readable" diary-file)))))

(defun calfw-cal-create-source-from-ical-url (name url &optional color auth-user)
  "Create diary calendar source."
 (lexical-let ((url url) (name name) (auth-user auth-user))
  (make-calfw-source
   :name (concat name)
   :color (or color "SaddleBrown")
   :data (lambda (begin end)
           (calfw-cal-to-calendar name url auth-user begin end))
   )))
          

(defun calfw-cal-view-diary ()
  "Show dairy on the selected date."
  (interactive)
  (let* ((cursor-date (calfw-cursor-to-nearest-date)))
    (my-diary-view-entry cursor-date)))

(defun calfw-cal-clear-cache-refresh () 
  "Clear cache and refresh all dairy source."
  (interactive )
  (let ((cp (calfw-cp-get-component)) (choice-list '(("all" 0))) (index 0) (name))
    (cl-loop for s in (calfw-cp-get-contents-sources cp)
          for f = (calfw-source-name s)
          if f do (push (list f index) choice-list))
    (setq name (completing-read "Select Cal to refresh: "
                                choice-list nil t "all"))
    (when (or (equal name "all") (equal name ""))
      (setq name "*"))
    (message "will clear caches %s" name)
    (call-process-shell-command (format "rm -rf %s/%s.*" calfw-cal-ical-url-cache-base name) nil "*Cal-Messages*"))
  (calfw-refresh-calendar-buffer nil))

;; 更新后 footer 上的日历从水平排列变成垂直排列了。。通override原来的函数，修改过来
(defun calfw--render-footer (_width sources)
  "Return a text of the footer.
The footer is rendered based on the SOURCES."
  (let* ((spaces (make-string 5 ? ))
         (whole-text
          (mapconcat
           'identity
           (cl-loop
            with keymap = (progn
                            (let ((kmap (make-sparse-keymap)))
                              (define-key kmap [mouse-1] 'calfw-event-mouse-click-toggle-calendar)
                              (define-key kmap [13] 'calfw-event-toggle-calendar)
                              kmap))
            for s in sources
            for hidden-p = (calfw-source-hidden s)
            for title = (calfw--tp (substring (calfw-source-name s) 0)
                                   'cfw:source s)
            for dot   = (calfw--tp (substring "(==)" 0) 'cfw:source s)
            collect
            (progn
              (calfw--tp dot 'mouse-face 'highlight)
              (propertize
               (calfw--render-default-content-face
                (concat
                 "[" (calfw--rt dot
                                (if hidden-p
                                    'calfw-calendar-hidden-face
                                  (calfw--render-get-face-period dot 'calfw-periods-face)))
                 " " title "]")
                (if hidden-p
                    'calfw-calendar-hidden-face
                  (calfw--render-get-face-content title
                                                  'calfw-default-content-face)))
               'keymap keymap)))
           (concat " " spaces)))) ;就是这里，原来是(concat "\n" spaces)
    (concat
     spaces
     whole-text)))

(keymap-set calfw-calendar-mode-map "d"  'calfw-cal-view-diary)
(keymap-set calfw-calendar-mode-map "R"  'calfw-cal-clear-cache-refresh)

(defun cal ()
  (interactive)
  (calfw-open-calendar-buffer
   :contents-sources
   (list
    (calfw-cal-create-source-from-ical-url "纪念日" "https://ical.madcat.cc/calanders/michael/89098fa4-ef29-7b1e-3cbd-09bc0ec16477/" "#ff0000" "michael")
    (calfw-cal-create-source-from-ical-url "旅行" "https://ical.madcat.cc/calanders/michael/4334dbe0-e104-cd7c-520c-9611ebff2e6e/" "#7D18F9" "michael")
    (calfw-cal-create-source-from-ical-url "娃校内" "https://ical.madcat.cc/calanders/michael/ab40a81c-48a6-3b5d-c5b2-b2f0653fea35/" "#2DEFF8" "michael")
    (calfw-cal-create-source-from-ical-url "娃校外" "https://ical.madcat.cc/calanders/michael/a47a0a99-a761-3126-e96b-f159ded1cf48/" "#7286DC" "michael")
    (calfw-cal-create-source-from-ical-url "学习计划" "https://ical.madcat.cc/calanders/michael/7ceac6da-9d09-4f33-fbe4-9a0332ae8e13/" "#03D74C" "michael")
    (calfw-cal-create-source-from-ical-url "医院" "https://ical.madcat.cc/calanders/michael/cbf482ec-b369-cdde-f982-ea984f45e49a/" "#FECC00" "michael")
    (calfw-cal-create-source-from-ical-url "其他" "https://ical.madcat.cc/calanders/michael/044b81cb-7f8d-15b7-9127-48ef1353150b/" "#D07669" "michael")
    (calfw-cal-create-source-from-ical-url "美国节日" "https://calendars.icloud.com/holidays/us_en-us.ics/" "#ff00ff")
    (calfw-cal-create-source-from-ical-url "中国节日" "https://calendars.icloud.com/holidays/cn_zh.ics/" "#0000cc")
    )))

;; 进入emacs的时候，默认显示日历
(setq initial-buffer-choice
      (lambda () (cal)
        (get-buffer "*cfw-calendar*")))

  
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

(provide 'config-calendar)
