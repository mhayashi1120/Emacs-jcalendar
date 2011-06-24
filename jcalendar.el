;;; -*- Coding: iso-2022-7bit -*-


;;; Commentary:
;;
;; 参考にした elisp ライブラリ
;; japanese-holidays.el
;; koyomi.el
;; http://www.kmc.gr.jp/~tak/sources/el/#koyomi

;;; TODO:
;; 節供、節気の一覧
;;

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
      ;; もし日曜日だったら平日
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
    ;; 一時的な休日
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

;;TODO 北朝
;; 明治の開始など和暦日付が突然別系統の暦に変わったときなど
(defconst jcalendar--era
  '(
    ("大化" (2 5 645))
    ("白雉" (2 9 650))
    ("朱鳥" (2 2 686))
    ("大宝" (2 17 701))
    ("慶雲" (2 14 704))
    ("和銅" (2 1 708))
    ("霊亀" (2 13 715))
    ("養老" (1 21 717))
    ("神亀" (2 4 724))
    ("天平" (2 7 729))
    ("天平感宝" (1 27 749))
    ("天平勝宝" (1 27 749))
    ("天平宝字" (1 29 757))
    ("天平神護" (1 30 765))
    ("神護景雲" (2 8 767))
    ("宝亀" (2 5 770))
    ("天応" (2 2 781))
    ("延暦" (1 22 782))
    ("大同" (1 28 806))
    ("弘仁" (2 12 810))
    ("天長" (2 8 824))
    ("承和" (2 16 834))
    ("嘉祥" (2 13 848))
    ("仁寿" (2 9 851))
    ("斉衡" (2 5 854))
    ("天安" (2 3 857))
    ("貞観" (2 11 859))
    ("元慶" (1 22 877))
    ("仁和" (1 24 885))
    ("寛平" (2 8 889))
    ("昌泰" (1 30 898))
    ("延喜" (1 28 901))
    ("延長" (1 25 923))
    ("承平" (1 27 931))
    ("天慶" (2 8 938))
    ("天暦" (1 30 947))
    ("天徳" (2 8 957))
    ("応和" (1 24 961))
    ("康保" (1 23 964))
    ("安和" (2 6 968))
    ("天禄" (2 14 970))
    ("天延" (2 11 973))
    ("貞元" (2 8 976))
    ("天元" (2 15 978))
    ("永観" (1 22 983))
    ("寛和" (1 29 985))
    ("永延" (2 6 987))
    ("永祚" (2 14 989))
    ("正暦" (2 3 990))
    ("長徳" (2 8 995))
    ("長保" (1 25 999))
    ("寛弘" (1 30 1004))
    ("長和" (2 1 1012))
    ("寛仁" (2 6 1017))
    ("治安" (1 22 1021))
    ("万寿" (2 18 1024))
    ("長元" (2 4 1028))
    ("長暦" (1 25 1037))
    ("長久" (1 23 1040))
    ("寛徳" (2 8 1044))
    ("永承" (2 15 1046))
    ("天喜" (1 29 1053))
    ("康平" (2 2 1058))
    ("治暦" (2 14 1065))
    ("延久" (2 1 1069))
    ("承保" (2 5 1074))
    ("承暦" (2 2 1077))
    ("永保" (2 18 1081))
    ("応徳" (2 15 1084))
    ("寛治" (2 12 1087))
    ("嘉保" (1 25 1094))
    ("永長" (2 3 1096))
    ("承徳" (1 22 1097))
    ("康和" (1 30 1099))
    ("長治" (2 5 1104))
    ("嘉承" (2 13 1106))
    ("天仁" (1 22 1108))
    ("天永" (1 29 1110))
    ("永久" (1 26 1113))
    ("元永" (1 31 1118))
    ("保安" (2 8 1120))
    ("天治" (1 25 1124))
    ("大治" (2 1 1126))
    ("天承" (2 7 1131))
    ("長承" (1 27 1132))
    ("保延" (1 23 1135))
    ("永治" (2 16 1141))
    ("康治" (2 5 1142))
    ("天養" (2 13 1144))
    ("久安" (2 1 1145))
    ("仁平" (1 27 1151))
    ("久寿" (1 23 1154))
    ("保元" (1 31 1156))
    ("平治" (1 28 1159))
    ("永暦" (2 16 1160))
    ("応保" (2 4 1161))
    ("長寛" (2 12 1163))
    ("永万" (1 21 1165))
    ("仁安" (2 9 1166))
    ("嘉応" (2 6 1169))
    ("承安" (2 14 1171))
    ("安元" (1 31 1175))
    ("治承" (2 8 1177))
    ("養和" (1 24 1181))
    ("寿永" (2 12 1182))
    ("元暦" (1 22 1184))
    ("文治" (2 9 1185))
    ("建久" (2 13 1190))
    ("正治" (2 4 1199))
    ("建仁" (2 12 1201))
    ("元久" (2 10 1204))
    ("建永" (2 17 1206))
    ("承元" (2 6 1207))
    ("建暦" (1 23 1211))
    ("建保" (1 31 1213))
    ("承久" (1 25 1219))
    ("貞応" (1 21 1222))
    ("元仁" (1 29 1224))
    ("嘉禄" (2 16 1225))
    ("安貞" (1 26 1227))
    ("寛喜" (2 3 1229))
    ("貞永" (1 31 1232))
    ("天福" (2 18 1233))
    ("文暦" (2 7 1234))
    ("嘉禎" (1 27 1235))
    ("暦仁" (1 25 1238))
    ("延応" (2 13 1239))
    ("仁治" (2 2 1240))
    ("寛元" (1 29 1243))
    ("宝治" (2 14 1247))
    ("建長" (1 22 1249))
    ("康元" (2 5 1256))
    ("正嘉" (1 24 1257))
    ("正元" (2 1 1259))
    ("文応" (1 21 1260))
    ("弘長" (2 8 1261))
    ("文永" (2 7 1264))
    ("建治" (2 5 1275))
    ("弘安" (2 1 1278))
    ("正応" (2 10 1288))
    ("永仁" (2 15 1293))
    ("正安" (2 9 1299))
    ("乾元" (2 7 1302))
    ("嘉元" (1 27 1303))
    ("徳治" (1 23 1306))
    ("延慶" (2 1 1308))
    ("応長" (1 28 1311))
    ("正和" (2 16 1312))
    ("文保" (1 22 1317))
    ("元応" (1 30 1319))
    ("元亨" (2 6 1321))
    ("正中" (2 4 1324))
    ("嘉暦" (2 11 1326))
    ("元徳" (2 8 1329))
    ("元弘(南朝)" (2 16 1331))
    ("建武(南朝)" (2 13 1334))
    ("延元(南朝)" (1 23 1336))
    ("興国(南朝)" (2 7 1340))
    ("正平(南朝)" (1 31 1346))
    ("建徳(南朝)" (2 5 1370))
    ("文中(南朝)" (2 13 1372))
    ("天授(南朝)" (2 9 1375))
    ("弘和(南朝)" (2 3 1381))
    ("元中(南朝)" (1 31 1384))
    ("応永" (2 9 1394))
    ("正長" (1 26 1428))
    ("永享" (2 13 1429))
    ("嘉吉" (2 1 1441))
    ("文安" (1 29 1444))
    ("宝徳" (2 2 1449))
    ("享徳" (1 31 1452))
    ("康正" (1 27 1455))
    ("長禄" (2 4 1457))
    ("寛正" (2 2 1460))
    ("文正" (1 26 1466))
    ("応仁" (2 14 1467))
    ("文明" (1 22 1469))
    ("長享" (2 3 1487))
    ("延徳" (2 10 1489))
    ("明応" (2 7 1492))
    ("文亀" (1 29 1501))
    ("永正" (1 27 1504))
    ("大永" (2 17 1521))
    ("享禄" (2 1 1528))
    ("天文" (2 16 1532))
    ("弘治" (2 2 1555))
    ("永禄" (1 30 1558))
    ("元亀" (2 15 1570))
    ("天正" (2 12 1573))
    ("文禄" (2 13 1592))
    ("慶長" (1 29 1596))
    ("元和" (1 29 1615))
    ("寛永" (2 19 1624))
    ("正保" (2 8 1644))
    ("慶安" (1 25 1648))
    ("承応" (2 10 1652))
    ("明暦" (2 6 1655))
    ("万治" (2 2 1658))
    ("寛文" (1 30 1661))
    ("延宝" (2 17 1673))
    ("天和" (2 18 1681))
    ("貞享" (2 15 1684))
    ("元禄" (2 2 1688))
    ("宝永" (2 5 1704))
    ("正徳" (2 17 1711))
    ("享保" (1 25 1716))
    ("元文" (2 12 1736))
    ("寛保" (2 16 1741))
    ("延享" (2 14 1744))
    ("寛延" (1 30 1748))
    ("宝暦" (1 27 1751))
    ("明和" (2 2 1764))
    ("安永" (2 4 1772))
    ("天明" (1 24 1781))
    ("寛政" (1 26 1789))
    ("享和" (2 13 1801))
    ("文化" (2 11 1804))
    ("文政" (2 5 1818))
    ("天保" (1 25 1830))
    ("弘化" (2 18 1844))
    ("嘉永" (2 5 1848))
    ("安政" (1 29 1854))
    ("万延" (1 23 1860))
    ("文久" (2 10 1861))
    ("元治" (2 8 1864))
    ("慶応" (1 27 1865))
    ("明治" ( 1 25 1868))
    ("大正" ( 7 30 1912))
    ("昭和" (12 25 1926))
    ("平成" ( 1  8 1989))
    ))

;; 日本語名で上書き
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
  "*todo"
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

(defun jcalendar--wareki-string (date)
  (let ((era (ignore-errors (jcalendar--date-to-era date))))
    (if era
        (format "%s%s年"
                (nth 0 era)
                (jcalendar-number-to-和暦 
                 (1+ (- (calendar-extract-year date)
                        (calendar-extract-year (nth 1 era))))))
      ;; TODO
      "未対応")))

(defun jcalendar-date-string (date)
  (let* ((year (calendar-extract-year date))
         (qreki (jcalendar--qreki date))
         (absolute (calendar-absolute-from-gregorian date)))
    (concat
     (format "和暦: %s" (jcalendar--wareki-string date))
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
     (format ", 六十干支: %s"
             (jcalendar--date-六十干支 absolute)))))

;; m1 は date 月の朔 m2 は date の次の朔
;;
;; m1 ------------ m2 --------------
;; |      date    |

;; m2 - m1 は 29 or 30 計算してみないと分からない

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
           (day (floor (- absolute saku)))
           (year (let* ((qm month)
                        (m (calendar-extract-month date))
                        (y (calendar-extract-year date)))
                   (loop for i in '(0 1 -1)
                         if (zerop (round (- (+ (/ (float m) 12) y) (+ (/ (float qm) 12) (+ y i)))))
                         return (+ y i)))))
      (list (1+ month) (1+ day) year leap))))

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
  (let ((jikkan (% n 10))
	(eto (% n 12)))
    (concat 
     (aref jcalendar--十干  jikkan) 
     (aref jcalendar--十二支 eto))))

(defun jcalendar--date-六十干支 (absolute)
  "ABSOLUTE (修正ユリウス通日) の六十干支を返す。"
  (jcalendar--六十干支 (+ absolute 14)))

;;TODO いらないかな
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
                    ;; trick
                    (n (if (> base 0) (/ n1 base) 0)))
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

(defun jcalendar-number-to-和暦 (number)
  (if (= number 1)
      "元"
    (jcalendar-number-to-kanji number)))


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

;; 月名は数字で読む。
(defadvice calendar-read-date
  (around jcalendar-read-date (&optional noday) activate)
  (let ((calendar-month-name-array
         (vconcat (loop for i from 1 to 12 
                        collect (number-to-string i)))))
    (setq ad-return-value ad-do-it)))

(jcalendar--initialize)

(provide 'jcalendar)
