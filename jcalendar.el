;;; -*- Coding: iso-2022-7bit -*-


;; japanese-holidays.el
;; koyomi.el
;; http://www.kmc.gr.jp/~tak/sources/el/#koyomi

(eval-when-compile
  (require 'cl))

(require 'calendar)
(require 'holidays)
(require 'lunar)
(require 'solar)
(require 'cal-julian)

(autoload 'solar-equinoxes-solstices "solar")

(defgroup jcalendar nil
  "Calendar extension for Japanese."
  :group 'jcalendar
  :prefix "jcalendar-")

(defvar jcalendar-general-holidays
  '(
    (holiday-fixed  1  1  "元日")
    (cond
     ;;todo
     ((or (>= displayed-year 2000)
	  (and (= displayed-year 1999) (> displayed-month 6)))
      (holiday-float 1 1 2 "成人の日"))
     (t
      (holiday-fixed 1 15 "成人の日")))
    (holiday-fixed  2 11  "建国記念の日")
    (cond 
     ((<= displayed-year 1988)
      (holiday-fixed 12 23  "天皇誕生日"))
     ((and (> displayed-year 1988) (< displayed-year 2007))
      (holiday-fixed  4 29  "みどりの日"))
     (t
      (holiday-fixed  4 29  "昭和の日")))
    (holiday-fixed  5  3  "憲法記念日")
    (cond
     ((and (> (calendar-day-of-week (list 5 4 displayed-year)) 0)
	   (>= displayed-year 1985)
	   (< displayed-year 2007))
      ;; if same as sunday then normalday...
      (holiday-fixed 5 4 "国民の休日"))
     ((>= displayed-year 2007)
      (holiday-fixed 5 4 "みどりの日")))
    (holiday-fixed  5  5  "こどもの日")
    (cond
     ((memq displayed-year '(2008 2009))
      (holiday-fixed 5 6 "振替休日")))
    (cond
     ((and (>= displayed-year 1996) (< displayed-year 2003))
      (holiday-fixed  7 20  "海の日"))
     ((>= displayed-year 2003)
      (holiday-float  7 1 3  "海の日")))
    (cond
     ((>= displayed-year 2003)
      (holiday-float  9 1 3  "敬老の日"))
     (t
      (holiday-fixed  9 15  "敬老の日")))
    (cond
     ((>= displayed-year 2000)
      (holiday-float 10 1 2 "体育の日"))
     (t
      (holiday-fixed 10 10 "体育の日")))
    (holiday-fixed 11  3  "文化の日")
    (holiday-fixed 11 23  "勤労感謝の日")
    (cond 
     ((> displayed-year 1988)
      (holiday-fixed 12 23  "天皇誕生日")))
    (if (memq displayed-month '(2 3 4 8 9 10))
	(solar-equinoxes-solstices))
    ;; temporary holiday
    (cond
     ((= displayed-year 1989)
      (holiday-fixed 2 24 "昭和天皇の大喪の礼"))
     ((= displayed-year 1990)
      (holiday-fixed 11 12 "即位礼正殿の儀"))
     ((= displayed-year 1993)
      (holiday-fixed 6 9 "皇太子徳仁親王の結婚の儀")))
    ;; exchange workday for holiday
    (when (not (boundp 'called-recursively))
      (let* (called-recursively
	     (month displayed-month)
	     (year displayed-year)
	     (day (progn
		    (increment-calendar-month month year -2)
		    (list month (calendar-last-day-of-month month year) year)))
	     (calendar-holidays jcalendar-general-holidays)
	     (holiday-list (calendar-holiday-list))
	     holiday mmday ret)
	(when (check-calendar-holidays day)
          (setq holiday-list (cons (list day "dummy") holiday-list)))
	(while holiday-list
	  (setq holiday (caar holiday-list))
	  (and (= (calendar-day-of-week holiday) 0)
	       (setq mmday (calendar-gregorian-from-absolute
			    (1+ (calendar-absolute-from-gregorian holiday))))
	       (null (check-calendar-holidays mmday))
	       (setq ret (cons (list mmday "振替休日") ret)))
	  (setq holiday-list (cdr holiday-list)))
	ret))))

(defconst jcalendar--rikuyou
  ["大安" "赤口" "先勝" "友引" "先負" "仏滅"])

(defconst jcalendar--era
  '(
    ("明治" ( 1 25 1868))
    ("大正" ( 7 30 1912))
    ("昭和" (12 25 1926))
    ("平成" ( 1  8 1989))
    ))

;; overwrite the default value.
(setq solar-n-hemi-seasons
      '("春分の日" "夏至" "秋分の日" "冬至"))

(defun jcalendar-fixed-furikae-holiday (m d s)
  (append (holiday-fixed m d s)
	  (and (= (calendar-day-of-week (list m d displayed-year)) 0)
	       (holiday-fixed m (1+ d) "振替休日"))))

(defun jcalendar-mark-saturday ()
  (jcalendar-mark-displayed-calendar
   6 'jcalendar-weekend-face))

(defun jcalendar-mark-sunday ()
  (jcalendar-mark-displayed-calendar
   0 'jcalendar-sunday-face))

(defmacro jcalendar--marking (&rest form)
  (declare (debug t) (indent 0))
  `(let (message-log-max)
     (message "Marking holidays...")
     (prog1 
         ,@form
       (message "Marking holidays...done"))))

(defun jcalendar-mark-special-days ()
  "Mark special days in the calendar window.
See `jcalendar-special-days'"
  (jcalendar--marking
    (jcalendar-set 'mark-holidays-in-calendar t)
    (let ((calendar-holidays jcalendar-special-days))
      (let ((holiday-list (calendar-holiday-list)))
        (while holiday-list
          (mark-visible-calendar-date
           (car (car holiday-list)) 'jcalendar-special-face)
          (setq holiday-list (cdr holiday-list)))))))

(defun jcalendar-mark-displayed-calendar (week-day face)
  "Mark all WEEK-DAY as FACE."
  (let ((m displayed-month)
	(y displayed-year))
    (increment-calendar-month m y -1)
    (calendar-for-loop
      i from 1 to 3 do
      (let ((sunday (- 1 (calendar-day-of-week (list m 1 y))))
            (last (calendar-last-day-of-month m y)))
        (while (<= sunday last)
          (let ((d (+ sunday week-day)))
            (and (<= 1 d)
                 (<= d last)
                 (mark-visible-calendar-date (list m d y) face)))
          (setq sunday (+ sunday 7))))
      (increment-calendar-month m y 1))))

;; for Emacs22
(cond
 ((boundp 'facemenu-unlisted-faces)
  (add-to-list 'facemenu-unlisted-faces 'jcalendar-sunday-face)
  (add-to-list 'facemenu-unlisted-faces 'jcalendar-special-face)
  (add-to-list 'facemenu-unlisted-faces 'jcalendar-weekend-face))
 (t
  (add-to-list 'facemenu-listed-faces 'jcalendar-weekend-face)
  (add-to-list 'facemenu-listed-faces 'jcalendar-special-face)
  (add-to-list 'facemenu-listed-faces 'jcalendar-weekend-face)
  ))

(defface jcalendar-weekend-face
  '((((class color) (background light))
     :foreground "Blue")
    (((class color) (background dark))
     :foreground "RoyalBlue1")
    (t
     :inverse-video t))
  "Face for indicating dates that have weekend."
  :group 'diary)

(defface jcalendar-sunday-face
  '((((class color) (background light))
     :foreground "Red")
    (((class color) (background dark))
     :foreground "DeepPink1")
    (t
     :inverse-video t))
  "Face for indicating dates that have sunday."
  :group 'diary)

(defface jcalendar-special-face
  '((((class color) (background light))
     :background "Deep sky blue")
    (((class color) (background dark))
     :background "Pale turquoise")
    (t
     :inverse-video t))
  "Face for indicating dates that is special day for you."
  :group 'diary)

(defcustom jcalendar-special-days nil
  "*"
  :group 'jcalendar)

(defun jcalendar--date-to-era (date)
  (let ((year (calendar-extract-year date))
        (month (calendar-extract-month date))
        (day (calendar-extract-day date)))
    (loop for era on jcalendar--era
          if (let ((start (nth 1 (car era)))
                   (end (nth 1 (cadr era))))
               (cond
                ((or (< year (calendar-extract-year start))
                     (and (= year (calendar-extract-year start))
                          (or (< month (calendar-extract-month start))
                              (and (= month (calendar-extract-month start))
                                   (< day (calendar-extract-day start))))))
                 (signal 'args-out-of-range date))
                ((null end) t)
                ((or (< year (calendar-extract-year end))
                     (and (= year (calendar-extract-year end))
                          (or (< month (calendar-extract-month end))
                              (and (= month (calendar-extract-month end))
                                   (< day (calendar-extract-day end))))))
                 t)
                (t nil)))
          return (car era))))

(defun jcalendar-print-date ()
  (interactive)
  (let ((date (calendar-cursor-to-date t)))
    (message "%s" (jcalendar-date-string date))))

(defun jcalendar-date-string (date)
  (let* ((era (jcalendar--date-to-era date))
         (year (calendar-extract-year date))
         (qreki (jcalendar--qreki date))
         (absolute (calendar-absolute-from-gregorian date)))
    (concat
     (format "和暦: %s%s年"
             (nth 0 era)
             (jcalendar-number-to-kanji 
              (1+ (- year (calendar-extract-year (nth 1 era))))))
     (let* ((m (calendar-extract-month qreki))
            (d (calendar-extract-day qreki)))
       (format ", 旧暦: %s%s月%s日, 六曜: %s" 
               (or (and (nth 3 qreki) "閏") "")
               (jcalendar-number-to-kanji m)
               (jcalendar-number-to-kanji d)
               (aref jcalendar--rikuyou (% (+ m d) 6))))
     (let ((sekku (jcalendar--sekku qreki)))
       (and sekku
            (format ", 節供: %s" sekku)))
     (let ((sekki (jcalendar--節気 absolute)))
       (and sekki
            (format ", 節気: %s" sekki)))
     (format ", 十干: %s"
             (jcalendar--date-六十干支 absolute)))))

;; m1 は date 月の朔 m2 は date の次の朔
;;
;; m1 ------------ m2 --------------
;; |      date    |

;; m2 - m1 は 29 or 30

(defun jcalendar--qreki (date)
  (let* ((absolute (calendar-absolute-from-gregorian date))
         (astro (calendar-astro-from-absolute absolute))
         (m2 (lunar-new-moon-on-or-after astro))
         m1)
    (if (= (floor absolute) (floor (calendar-absolute-from-astro m2)))
        ;; 引数 date が朔
        (setq m1 m2
              m2 (lunar-new-moon-on-or-after (1+ astro)))
      (setq m1 (lunar-new-moon-on-or-after (- astro 29)))
      (when (= (floor m1) (floor m2))
        (setq m1 (lunar-new-moon-on-or-after (- astro 30)))))
    ;; m1, m2 を日単位まで切り捨てる
    (let* ((saku (floor (calendar-absolute-from-astro m1)))
           (next-saku (floor (calendar-absolute-from-astro m2)))
           (longitude (solar-date-next-longitude (calendar-astro-from-absolute saku) 30))
           (leap (>= (calendar-absolute-from-astro longitude) next-saku))
           (month (% (+ (round (solar-longitude longitude) 30) (if leap 0 1)) 12))
           (day (floor (- absolute saku))))
      (list (1+ month) (1+ day) nil leap))))

(defconst jcalendar--節供
  '(("元日" 1 1)
    ("人日 (七草の節供)" 1 7)
    ("小正月" 1 15)
    ("上巳 (桃の節供 雛祭)" 3 3)
    ("端午 (菖蒲の節供)" 5 5)
    ("七夕" 7 7)
    ("盆" 7 15)
    ("重陽 (菊の節供)" 9 9)))

(defun jcalendar--sekku (qreki)
  (let ((m (calendar-extract-month qreki))
        (d (calendar-extract-day qreki))
        (leap (nth 3 qreki)))
    (unless leap
      (car (find-if 
            (lambda (x) (and (= (nth 1 x) m)
                             (= (nth 2 x) d)))
            jcalendar--節供)))))

(defconst jcalendar--五行
  ["木" "火" "土" "金" "水"])

(defconst jcalendar--十二支 
  ["子" "丑" "寅" "卯" "辰" "巳"
   "午" "未" "申" "酉" "戌" "亥"])

(defconst jcalendar--十干 
  ["甲" "乙" "丙" "丁" "戊" "己" "庚" "辛" "壬" "癸"])

(defun jcalendar--六十干支 (n)
  "N の六十干支を返す。"
  (let ((十干 (% n 10))
	(十二支 (% n 12)))
    (concat 
     (aref jcalendar--十干  十干) 
     (aref jcalendar--十二支 十二支))))

(defun jcalendar--date-六十干支 (absolute)
  "ABSOLUTE (修正ユリウス通日) の六十干支を返す。"
  (jcalendar--六十干支 (+ absolute 14)))

;;TODO いくらなんでもいらなくね？
(defun jcalendar--year-六十干支 (year)
  "YEAR (西暦) の六十干支を返す。"
  (jcalendar--六十干支 (+ year 56)))

(defconst jcalendar--二十四節気
  ["春分" "清明" "穀雨" "立夏" "小満" "芒種"
   "夏至" "小暑" "大暑" "立秋" "処暑" "白露"
   "秋分" "寒露" "霜降" "立冬" "小雪" "大雪"
   "冬至" "小寒" "大寒" "立春" "雨水" "啓蟄" ])

(defun jcalendar--節気-1 (jd longitude &optional mod90)
  "JD (ユリウス通日) が太陽黄経 LONGITUDE を通るとき t を返す。

`jcalendar--節気' のサブルーチン。"
  (let ((today (solar-longitude jd))
	(tomorrow (solar-longitude (1+ jd))))
    (if mod90
	(setq today (mod today 90)
	      tomorrow (mod tomorrow 90)))
    (and (<= (if (< today tomorrow) today (- today (if mod90 90 360))) longitude)
	 (< longitude tomorrow))))

(defun jcalendar--節気 (absolute)
  "ABSOLUTE (修正ユリウス通日) が節気に相当する場合はそれを返す。

節気以外にも、太陽黄経から求められる雑節などの暦日があれば返す。
該当する暦日がなければ nil を返す。"
  (let* ((astro (calendar-astro-from-absolute (floor absolute)))
	 (today (solar-longitude astro))
	 (tomorrow (solar-longitude (1+ astro)))
	 (index (round today 15))
	 (today-90 (mod today 90)))
    (cond ((and (<= today (* index 15))
		(< (* index 15) (if (> today tomorrow) (+ 360 tomorrow) tomorrow)))
	   (aref jcalendar--二十四節気 (% index 24)))
	  ((and (<= today  80) (<  80 tomorrow)) "入梅")
	  ((and (<= today 100) (< 100 tomorrow)) "半夏生")
	  ((and (<  40 today) (< today  45) (jcalendar--節気-1 (- astro  87) 315))
	   "八十八夜")
	  ((and (< 155 today) (< today 165) (jcalendar--節気-1 (- astro 209) 315))
	   "二百十日")
	  ((and (< 165 today) (< today 175) (jcalendar--節気-1 (- astro 219) 315))
	   "二百二十日")
	  ((and (or (< today 5)
		    (and (< 180 today) (< today 185)))
		(cond ((< 175 (mod (solar-longitude (- astro 2)) 180))
		       "彼岸")
		      ((< 175 (mod (solar-longitude (- astro 3)) 180))
		       "彼岸明け"))))
	  ((and (or (and (< 175 today) (< today 180))
		    (< 355 today))
		(cond ((< (mod (solar-longitude (+ astro 3)) 180) 5)
		       "彼岸")
		      ((< (mod (solar-longitude (+ astro 4)) 180) 5)
		       "彼岸入り"))))
	  ;; 現在では、土用として「立[春夏秋冬]の直前18日間」よりも
	  ;; 「太陽が黄経 (90n + 27)°を通過する日から立[春夏秋冬]の前日まで」
	  ;; という定義が一般に用いられているようだ
	  ;;((and (< 25 today-90) (< today-90 30) (jcalendar--節気-1 (+ astro 18) 45 t))
	  ;; "土用")
	  ((and (<= today-90 27) (< 27 (mod tomorrow 90))) "土用入り")
	  ((and (< 27 today-90) (< today-90 45))
	   (if (and (< 43 today-90) (jcalendar--節気-1 (1+ astro) 45 t))
	       "節分"
	     "土用")))))

(defconst jcalendar--漢数字 [nil ?一 ?二 ?三 ?四 ?五 ?六 ?七 ?八 ?九])
(defconst jcalendar--漢数字-位 [nil ?十 ?百 ?千])
(defconst jcalendar--漢数字-位2 [nil ?万 ?億 ?兆 ?京])

(defun jcalendar-number-to-kanji (number)
  (when (minusp number)
    (signal 'args-out-of-range (list number)))
  (if (zerop number)
      "零"
    (loop with res 
          with n1 = number
          for d1 in '(8 4 0)
          for i downfrom 2
          do (let* ((base (expt 10 d1))
                    (n (/ n1 base)))
               (when (> n 0)
                 (loop with n2 = n
                       for d2 downfrom 3 to 0
                       do (let* ((base (expt 10 d2))
                                 (n (/ n2 base))
                                 (digit (aref jcalendar--漢数字 n)))
                            (when digit
                              (let* ((geta (aref jcalendar--漢数字-位 d2))
                                     (keta (cond
                                            ((and (= n 1) (>= d2 1))
                                             (when geta
                                               (list geta)))
                                            (geta 
                                             (list geta digit))
                                            (t
                                             (list digit)))))
                                (setq res (append keta res))))
                            (setq n2 (% n2 base))))
                 (let ((geta (aref jcalendar--漢数字-位2 i)))
                   (when geta
                     (setq res (cons geta res)))))
               (setq n1 (% n1 base)))
          finally return (concat (nreverse res)))))

;; TODO ひのととか

;;
;; inner functions
;;

(defmacro jcalendar-set (sym val)
  `(set (jcalendar--variable ,sym) ,val))

(defun jcalendar--function (symbol)
  (let ((prop (get symbol 'byte-obsolete-info)))
    (or (car prop) symbol)))

(defun jcalendar--variable (symbol)
  (let ((prop (get symbol 'byte-obsolete-variable)))
    (or (car prop) symbol)))

(defun jcalendar--add-hook (hook function)
  (add-hook (jcalendar--variable hook) 
            (or
             (and (symbolp function) (jcalendar--function function))
             function)))

(defun jcalendar--initialize ()
  (define-key calendar-mode-map "pJ" 'jcalendar-print-date)
  
  ;; mark today
  (jcalendar--add-hook 'today-visible-calendar-hook 'calendar-mark-today)

  ;; mark week end
  (jcalendar--add-hook 'today-visible-calendar-hook 'jcalendar-mark-sunday)
  (jcalendar--add-hook 'today-visible-calendar-hook 'jcalendar-mark-saturday)
  (jcalendar--add-hook 'today-invisible-calendar-hook 'jcalendar-mark-sunday)
  (jcalendar--add-hook 'today-invisible-calendar-hook 'jcalendar-mark-saturday)

  ;; mark not week end holidays
  (jcalendar--add-hook 'today-visible-calendar-hook 'mark-calendar-holidays)
  (jcalendar--add-hook 'today-invisible-calendar-hook 'mark-calendar-holidays)

  (jcalendar--add-hook 'today-visible-calendar-hook 'jcalendar-mark-special-days)
  (jcalendar--add-hook 'today-invisible-calendar-hook 'jcalendar-mark-special-days))

(jcalendar--initialize)

(provide 'jcalendar)
