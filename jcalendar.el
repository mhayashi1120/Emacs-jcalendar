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
    (holiday-fixed  1  1  "$B85F|(B")
    (cond
     ;;todo
     ((or (>= displayed-year 2000)
	  (and (= displayed-year 1999) (> displayed-month 6)))
      (holiday-float 1 1 2 "$B@.?M$NF|(B"))
     (t
      (holiday-fixed 1 15 "$B@.?M$NF|(B")))
    (holiday-fixed  2 11  "$B7z9q5-G0$NF|(B")
    (cond 
     ((<= displayed-year 1988)
      (holiday-fixed 12 23  "$BE79DCB@8F|(B"))
     ((and (> displayed-year 1988) (< displayed-year 2007))
      (holiday-fixed  4 29  "$B$_$I$j$NF|(B"))
     (t
      (holiday-fixed  4 29  "$B><OB$NF|(B")))
    (holiday-fixed  5  3  "$B7{K!5-G0F|(B")
    (cond
     ((and (> (calendar-day-of-week (list 5 4 displayed-year)) 0)
	   (>= displayed-year 1985)
	   (< displayed-year 2007))
      ;; if same as sunday then normalday...
      (holiday-fixed 5 4 "$B9qL1$N5YF|(B"))
     ((>= displayed-year 2007)
      (holiday-fixed 5 4 "$B$_$I$j$NF|(B")))
    (holiday-fixed  5  5  "$B$3$I$b$NF|(B")
    (cond
     ((memq displayed-year '(2008 2009))
      (holiday-fixed 5 6 "$B?6BX5YF|(B")))
    (cond
     ((and (>= displayed-year 1996) (< displayed-year 2003))
      (holiday-fixed  7 20  "$B3$$NF|(B"))
     ((>= displayed-year 2003)
      (holiday-float  7 1 3  "$B3$$NF|(B")))
    (cond
     ((>= displayed-year 2003)
      (holiday-float  9 1 3  "$B7IO7$NF|(B"))
     (t
      (holiday-fixed  9 15  "$B7IO7$NF|(B")))
    (cond
     ((>= displayed-year 2000)
      (holiday-float 10 1 2 "$BBN0i$NF|(B"))
     (t
      (holiday-fixed 10 10 "$BBN0i$NF|(B")))
    (holiday-fixed 11  3  "$BJ82=$NF|(B")
    (holiday-fixed 11 23  "$B6PO+46<U$NF|(B")
    (cond 
     ((> displayed-year 1988)
      (holiday-fixed 12 23  "$BE79DCB@8F|(B")))
    (if (memq displayed-month '(2 3 4 8 9 10))
	(solar-equinoxes-solstices))
    ;; temporary holiday
    (cond
     ((= displayed-year 1989)
      (holiday-fixed 2 24 "$B><OBE79D$NBgAS$NNi(B"))
     ((= displayed-year 1990)
      (holiday-fixed 11 12 "$BB(0LNi@5EB$N57(B"))
     ((= displayed-year 1993)
      (holiday-fixed 6 9 "$B9DB@;RFA?N?F2&$N7k:'$N57(B")))
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
	       (setq ret (cons (list mmday "$B?6BX5YF|(B") ret)))
	  (setq holiday-list (cdr holiday-list)))
	ret))))

(defconst jcalendar--rikuyou
  ["$BBg0B(B" "$B@V8}(B" "$B@h>!(B" "$BM'0z(B" "$B@hIi(B" "$BJ)LG(B"])

(defconst jcalendar--era
  '(
    ("$BL@<#(B" ( 1 25 1868))
    ("$BBg@5(B" ( 7 30 1912))
    ("$B><OB(B" (12 25 1926))
    ("$BJ?@.(B" ( 1  8 1989))
    ))

;; overwrite the default value.
(setq solar-n-hemi-seasons
      '("$B=UJ,$NF|(B" "$B2F;j(B" "$B=)J,$NF|(B" "$BE_;j(B"))

(defun jcalendar-fixed-furikae-holiday (m d s)
  (append (holiday-fixed m d s)
	  (and (= (calendar-day-of-week (list m d displayed-year)) 0)
	       (holiday-fixed m (1+ d) "$B?6BX5YF|(B"))))

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
     (format "$BOBNq(B: %s%s$BG/(B"
             (nth 0 era)
             (jcalendar-number-to-kanji 
              (1+ (- year (calendar-extract-year (nth 1 era))))))
     (let* ((m (calendar-extract-month qreki))
            (d (calendar-extract-day qreki)))
       (format ", $B5lNq(B: %s%s$B7n(B%s$BF|(B, $BO;MK(B: %s" 
               (or (and (nth 3 qreki) "$B1<(B") "")
               (jcalendar-number-to-kanji m)
               (jcalendar-number-to-kanji d)
               (aref jcalendar--rikuyou (% (+ m d) 6))))
     (let ((sekku (jcalendar--sekku qreki)))
       (and sekku
            (format ", $B@a6!(B: %s" sekku)))
     (let ((sekki (jcalendar--$B@a5$(B absolute)))
       (and sekki
            (format ", $B@a5$(B: %s" sekki)))
     (format ", $B==43(B: %s"
             (jcalendar--date-$BO;==43;Y(B absolute)))))

;; m1 $B$O(B date $B7n$N:s(B m2 $B$O(B date $B$N<!$N:s(B
;;
;; m1 ------------ m2 --------------
;; |      date    |

;; m2 - m1 $B$O(B 29 or 30

(defun jcalendar--qreki (date)
  (let* ((absolute (calendar-absolute-from-gregorian date))
         (astro (calendar-astro-from-absolute absolute))
         (m2 (lunar-new-moon-on-or-after astro))
         m1)
    (if (= (floor absolute) (floor (calendar-absolute-from-astro m2)))
        ;; $B0z?t(B date $B$,:s(B
        (setq m1 m2
              m2 (lunar-new-moon-on-or-after (1+ astro)))
      (setq m1 (lunar-new-moon-on-or-after (- astro 29)))
      (when (= (floor m1) (floor m2))
        (setq m1 (lunar-new-moon-on-or-after (- astro 30)))))
    ;; m1, m2 $B$rF|C10L$^$G@Z$j<N$F$k(B
    (let* ((saku (floor (calendar-absolute-from-astro m1)))
           (next-saku (floor (calendar-absolute-from-astro m2)))
           (longitude (solar-date-next-longitude (calendar-astro-from-absolute saku) 30))
           (leap (>= (calendar-absolute-from-astro longitude) next-saku))
           (month (% (+ (round (solar-longitude longitude) 30) (if leap 0 1)) 12))
           (day (floor (- absolute saku))))
      (list (1+ month) (1+ day) nil leap))))

(defconst jcalendar--$B@a6!(B
  '(("$B85F|(B" 1 1)
    ("$B?MF|(B ($B<7Ap$N@a6!(B)" 1 7)
    ("$B>.@57n(B" 1 15)
    ("$B>eL&(B ($BEm$N@a6!(B $B?w:W(B)" 3 3)
    ("$BC<8a(B ($B>T3w$N@a6!(B)" 5 5)
    ("$B<7M<(B" 7 7)
    ("$BK_(B" 7 15)
    ("$B=EM[(B ($B5F$N@a6!(B)" 9 9)))

(defun jcalendar--sekku (qreki)
  (let ((m (calendar-extract-month qreki))
        (d (calendar-extract-day qreki))
        (leap (nth 3 qreki)))
    (unless leap
      (car (find-if 
            (lambda (x) (and (= (nth 1 x) m)
                             (= (nth 2 x) d)))
            jcalendar--$B@a6!(B)))))

(defconst jcalendar--$B8^9T(B
  ["$BLZ(B" "$B2P(B" "$BEZ(B" "$B6b(B" "$B?e(B"])

(defconst jcalendar--$B==Fs;Y(B 
  ["$B;R(B" "$B1/(B" "$BFR(B" "$B1,(B" "$BC$(B" "$BL&(B"
   "$B8a(B" "$BL$(B" "$B?=(B" "$BFS(B" "$BX|(B" "$B0g(B"])

(defconst jcalendar--$B==43(B 
  ["$B9C(B" "$B25(B" "$BJ:(B" "$BCz(B" "$BJj(B" "$B8J(B" "$B9.(B" "$B?I(B" "$B?Q(B" "$Bb#(B"])

(defun jcalendar--$BO;==43;Y(B (n)
  "N $B$NO;==43;Y$rJV$9!#(B"
  (let (($B==43(B (% n 10))
	($B==Fs;Y(B (% n 12)))
    (concat 
     (aref jcalendar--$B==43(B  $B==43(B) 
     (aref jcalendar--$B==Fs;Y(B $B==Fs;Y(B))))

(defun jcalendar--date-$BO;==43;Y(B (absolute)
  "ABSOLUTE ($B=$@5%f%j%&%9DLF|(B) $B$NO;==43;Y$rJV$9!#(B"
  (jcalendar--$BO;==43;Y(B (+ absolute 14)))

;;TODO $B$$$/$i$J$s$G$b$$$i$J$/$M!)(B
(defun jcalendar--year-$BO;==43;Y(B (year)
  "YEAR ($B@>Nq(B) $B$NO;==43;Y$rJV$9!#(B"
  (jcalendar--$BO;==43;Y(B (+ year 56)))

(defconst jcalendar--$BFs==;M@a5$(B
  ["$B=UJ,(B" "$B@6L@(B" "$B9r1+(B" "$BN)2F(B" "$B>.K~(B" "$Bgj<o(B"
   "$B2F;j(B" "$B>.=k(B" "$BBg=k(B" "$BN)=)(B" "$B=h=k(B" "$BGrO*(B"
   "$B=)J,(B" "$B4(O*(B" "$BAz9_(B" "$BN)E_(B" "$B>.@c(B" "$BBg@c(B"
   "$BE_;j(B" "$B>.4((B" "$BBg4((B" "$BN)=U(B" "$B1+?e(B" "$B7<j/(B" ])

(defun jcalendar--$B@a5$(B-1 (jd longitude &optional mod90)
  "JD ($B%f%j%&%9DLF|(B) $B$,B@M[2+7P(B LONGITUDE $B$rDL$k$H$-(B t $B$rJV$9!#(B

`jcalendar--$B@a5$(B' $B$N%5%V%k!<%A%s!#(B"
  (let ((today (solar-longitude jd))
	(tomorrow (solar-longitude (1+ jd))))
    (if mod90
	(setq today (mod today 90)
	      tomorrow (mod tomorrow 90)))
    (and (<= (if (< today tomorrow) today (- today (if mod90 90 360))) longitude)
	 (< longitude tomorrow))))

(defun jcalendar--$B@a5$(B (absolute)
  "ABSOLUTE ($B=$@5%f%j%&%9DLF|(B) $B$,@a5$$KAjEv$9$k>l9g$O$=$l$rJV$9!#(B

$B@a5$0J30$K$b!"B@M[2+7P$+$i5a$a$i$l$k;(@a$J$I$NNqF|$,$"$l$PJV$9!#(B
$B3:Ev$9$kNqF|$,$J$1$l$P(B nil $B$rJV$9!#(B"
  (let* ((astro (calendar-astro-from-absolute (floor absolute)))
	 (today (solar-longitude astro))
	 (tomorrow (solar-longitude (1+ astro)))
	 (index (round today 15))
	 (today-90 (mod today 90)))
    (cond ((and (<= today (* index 15))
		(< (* index 15) (if (> today tomorrow) (+ 360 tomorrow) tomorrow)))
	   (aref jcalendar--$BFs==;M@a5$(B (% index 24)))
	  ((and (<= today  80) (<  80 tomorrow)) "$BF~G_(B")
	  ((and (<= today 100) (< 100 tomorrow)) "$BH>2F@8(B")
	  ((and (<  40 today) (< today  45) (jcalendar--$B@a5$(B-1 (- astro  87) 315))
	   "$BH,==H,Lk(B")
	  ((and (< 155 today) (< today 165) (jcalendar--$B@a5$(B-1 (- astro 209) 315))
	   "$BFsI4==F|(B")
	  ((and (< 165 today) (< today 175) (jcalendar--$B@a5$(B-1 (- astro 219) 315))
	   "$BFsI4Fs==F|(B")
	  ((and (or (< today 5)
		    (and (< 180 today) (< today 185)))
		(cond ((< 175 (mod (solar-longitude (- astro 2)) 180))
		       "$BH`4_(B")
		      ((< 175 (mod (solar-longitude (- astro 3)) 180))
		       "$BH`4_L@$1(B"))))
	  ((and (or (and (< 175 today) (< today 180))
		    (< 355 today))
		(cond ((< (mod (solar-longitude (+ astro 3)) 180) 5)
		       "$BH`4_(B")
		      ((< (mod (solar-longitude (+ astro 4)) 180) 5)
		       "$BH`4_F~$j(B"))))
	  ;; $B8=:_$G$O!"EZMQ$H$7$F!VN)(B[$B=U2F=)E_(B]$B$ND>A0(B18$BF|4V!W$h$j$b(B
	  ;; $B!VB@M[$,2+7P(B (90n + 27)$B!k$rDL2a$9$kF|$+$iN)(B[$B=U2F=)E_(B]$B$NA0F|$^$G!W(B
	  ;; $B$H$$$&Dj5A$,0lHL$KMQ$$$i$l$F$$$k$h$&$@(B
	  ;;((and (< 25 today-90) (< today-90 30) (jcalendar--$B@a5$(B-1 (+ astro 18) 45 t))
	  ;; "$BEZMQ(B")
	  ((and (<= today-90 27) (< 27 (mod tomorrow 90))) "$BEZMQF~$j(B")
	  ((and (< 27 today-90) (< today-90 45))
	   (if (and (< 43 today-90) (jcalendar--$B@a5$(B-1 (1+ astro) 45 t))
	       "$B@aJ,(B"
	     "$BEZMQ(B")))))

(defconst jcalendar--$B4A?t;z(B [nil ?$B0l(B ?$BFs(B ?$B;0(B ?$B;M(B ?$B8^(B ?$BO;(B ?$B<7(B ?$BH,(B ?$B6e(B])
(defconst jcalendar--$B4A?t;z(B-$B0L(B [nil ?$B==(B ?$BI4(B ?$B@i(B])
(defconst jcalendar--$B4A?t;z(B-$B0L(B2 [nil ?$BK|(B ?$B2/(B ?$BC{(B ?$B5~(B])

(defun jcalendar-number-to-kanji (number)
  (when (minusp number)
    (signal 'args-out-of-range (list number)))
  (if (zerop number)
      "$BNm(B"
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
                                 (digit (aref jcalendar--$B4A?t;z(B n)))
                            (when digit
                              (let* ((geta (aref jcalendar--$B4A?t;z(B-$B0L(B d2))
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
                 (let ((geta (aref jcalendar--$B4A?t;z(B-$B0L(B2 i)))
                   (when geta
                     (setq res (cons geta res)))))
               (setq n1 (% n1 base)))
          finally return (concat (nreverse res)))))

;; TODO $B$R$N$H$H$+(B

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
