;; -*- lexical-binding: t; -*-
;;;;;;;;;;;;;;;;获取天气
(require 'wttrin)
(require 'cl-lib)

;; 设置默认城市
(setq wttrin-default-cities '("Beijing"))

;; 设置默认语言
(setq wttrin-default-accept-language '("Accept-Language" . "zh-CN"))

(defvar get-city-weather--translation-alist
  '(("sunny" . "晴") ("clear" . "晴")
    ("partly cloudy" . "多云") ("cloudy" . "多云") ("overcast" . "阴")
    ("mist" . "雾") ("fog" . "雾") ("freezing fog" . "冻雾")
    ("haze" . "霾") ("smoky haze" . "霾") ("dust" . "浮尘") ("sand" . "扬沙")
    ("patchy rain nearby" . "局部阵雨") ("patchy rain possible" . "阵雨")
    ("light drizzle" . "毛毛雨") ("patchy light drizzle" . "毛毛雨")
    ("freezing drizzle" . "冻雨") ("heavy freezing drizzle" . "强冻雨")
    ("light rain" . "小雨") ("light rain shower" . "阵雨")
    ("moderate rain" . "中雨") ("moderate rain at times" . "中雨")
    ("heavy rain" . "大雨") ("heavy rain at times" . "大雨")
    ("torrential rain shower" . "暴雨")
    ("patchy snow nearby" . "阵雪") ("patchy snow possible" . "阵雪")
    ("light snow" . "小雪") ("light snow shower" . "阵雪") ("patchy light snow" . "小雪")
    ("moderate snow" . "中雪") ("heavy snow" . "大雪")
    ("blizzard" . "暴风雪") ("blowing snow" . "飞雪")
    ("sleet" . "雨夹雪") ("light sleet" . "小雨夹雪") ("light sleet showers" . "阵雨夹雪")
    ("ice pellets" . "冰粒") ("light showers of ice pellets" . "阵性冰粒")
    ("thundery outbreaks possible" . "雷阵雨")
    ("patchy light rain with thunder" . "雷阵雨")
    ("moderate or heavy rain with thunder" . "强雷阵雨")
    ("patchy light snow with thunder" . "雷阵雪")
    ("moderate or heavy snow with thunder" . "大雷阵雪"))
  "英文天气到中文简短字符的映射表。")

(defun get-city-weather--translate (raw-text)
  "将原始英文天气描述转换为中文描述，包含模糊兜底。"
  (let* ((text (string-trim (downcase raw-text)))
         (exact-match (assoc text get-city-weather--translation-alist)))
    (cond
     ;; 1. 优先精确匹配字典
     (exact-match (cdr exact-match))
     ;; 2. 严格的模糊匹配兜底（两边加上词边界或单独匹配，防止 curl 报错信息误伤）
     ((string-match-p "thunder" text) "雷阵雨")
     ((or (string-match-p "snow" text) (string-match-p "blizzard" text)) "雪")
     ((or (string-match-p "rain" text) (string-match-p "drizzle" text)) "雨")
     ((string-match-p "sleet" text) "雨夹雪")
     ((string-match-p "fog" text) "雾")
     ((string-match-p "haze" text) "霾")
     ((string-match-p "cloudy" text) "多云")
     ((or (string-match-p "clear" text) (string-match-p "sunny" text)) "晴")
     ((string-match-p "overcast" text) "阴")
     ;; 3. 实在没有匹配到，返回原始英文首字母大写
     (t (capitalize raw-text)))))


(defun get-city-weather-string (city)
  "输入城市拼音或英文名称（如 \"Shanghai\"），
调用系统 curl 抓取 wttr.in 并直接返回映射后的中文天气字符串。
若发生网络错误、命令失败或解析异常，则静默返回 \"未知天气\"。"
  (condition-case nil
      (let* ((formatted-city (capitalize (string-trim city)))
             (encoded-city (url-hexify-string formatted-city))
             (url (format "https://wttr.in/%s?format=%%C" encoded-city))
             (cmd (format "curl -s -f -A \"curl\" \"%s\"" url))
             (raw-weather (shell-command-to-string cmd))
             (exit-code (shell-command (format "%s > /dev/null 2>&1" cmd))))
        
        ;; 验证网络及命令执行状态
        (if (or (not (eq exit-code 0)) 
                (string-empty-p (string-trim raw-weather))
                (string-match-p "curl:" (downcase raw-weather))
                ;; 验证是否返回了网页
                (string-match-p "<html" (downcase raw-weather)))
            "未知天气"
          ;; 直接返回解析翻译后的纯文本字符串
          (get-city-weather--translate raw-weather)))
    ;; 捕获可能产生的任何意料之外的 Lisp 错误，一律返回未知天气
    (error "未知天气")))

(defun get-city-weather (city)
  "【交互式命令】输入城市拼音/英文（如 \"Beijing\"），
通过 `get-city-weather-string' 获取天气，并在 Echo Area 打印显示。"
  (interactive "s输入城市拼音/英文: ")
  (let ((weather-str (get-city-weather-string city)))
    (message "当前天气: %s" weather-str)
    weather-str))

(provide 'config-weather)
