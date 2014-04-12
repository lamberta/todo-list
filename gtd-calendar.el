;;
;; Calendar Add Event
;;

(defcustom gtd-calendars nil
  "List of available calendars. This is used for prompting the user for a calendar selection, the first being the default."
  :type 'list
  :group 'gtd-calendar)

(defcustom gtd-calendar-action nil
  "Function to call when adding a calendar event. This is system dependant."
  :type 'function
  :group 'gtd-calendar)


;;parse-time: http://ergoemacs.org/emacs/elisp_parse_time.html
;;apple format: Apr 11, 2014 6:30:00 PM
(defun gtd-add-calendar-event (&optional cal-name title start end all-day-p description location)
  "Return T on success or NIL on failure."
  (interactive)
  (if (called-interactively-p 'interactive)
    (setq
      title (read-string "Event title: ")
      cal-name (gtd-completing-read gtd-calendars "Calendar" (car gtd-calendars))
      start (read-string "Start time xxx-xxx-x: ")
      end (read-string "End time xxx-xxx-xx: ")
      all-day-p (yes-or-no-p "All day event? ")))
  (let (err-msg)
    (cond
      ;;check args
      ((not (member cal-name gtd-calendars))
	(setq err-msg (format "Could not find '%s' in `gtd-calendars'." cal-name)))
      ((not (valid-entry-p title))
	(setq err-msg "Invalid title name entry."))
      ((not (valid-entry-p start))
	(setq err-msg "Invalid start time entry."))
      ;;check systems
      ((eq system-type 'darwin) ;osx
	(let* ((time-format "%b %d, %Y %I:%M %p") ;"%b %d, %Y %T"
		(start-osx (format-time-string time-format (gtd-encode-time-string start)))
		(end-osx (format-time-string time-format (gtd-encode-time-string end)))
		(cal-script (format-osx-event-script cal-name title start-osx end-osx all-day-p description location)))
	  (message "-->|%s|%s|%s" start-osx end-osx time-format)
	  (assert (stringp start-osx))
	  (assert (stringp cal-script))
	  (start-process "osx-calendar-add-event" nil "/usr/bin/osascript" "-e" cal-script)))
      (t
	(setq err-msg "Not implemented on this system.")))
    ;;write status
    (let ((title-short (if (> (length title) 25)
			 (concat (trim-string (substring title 0 22)) "...")
			 title)))
      (message (or err-msg (format "Added '%s' to '%s' calendar." title-short cal-name))))
    (not err-msg)))

;;
;; PARSE DATETIME
;;

(defun gtd-encode-time-string (time-string)
  "Parse the TIME-STRING and apply reasonable defaults. Return the encoded time."
  (let* ((time-period
	   (gtd-parse-time-period
	     (gtd-fix-dangling-time time-string)))
	 (time-str (gtd-time-natural-lang-replace (first time-period)))
	 (pm-p (second time-period))
	 (time-list (butlast (parse-time-string time-str) 3)))
    (if (not (delq t (mapcar #'null time-list)))
      (progn
	(message "Unable to parse time string: %s" time-string)
	nil)
      (let* ((cur-time-list (butlast (parse-time-string (current-time-string)) 3))
	     (s (or (nth 0 time-list) 0))
	     (m (or (nth 1 time-list) 0))
	     (h (or (nth 2 time-list) 0))
	     (d (or (nth 3 time-list) (nth 3 cur-time-list)))
	     (mon (or (nth 4 time-list) ;default this month, unless day is less than today, then next month
		    (let ((cur-d (nth 3 cur-time-list))
			  (cur-mon (nth 4 cur-time-list)))
		      (if (< d cur-d)
			(if (= cur-mon 12) 1 (1+ cur-mon))
			cur-mon))))
	      (y (or (nth 5 time-list) ;default this year, unless mon is less that this one, then next year
		   (let ((cur-mon (nth 4 cur-time-list))
			 (cur-y (nth 5 cur-time-list)))
		     (if (< mon cur-mon)
		       (1+ cur-y)
		       cur-y)))))
	(if pm-p
	  (cond
	    ((< h 11) (setq h (+ h 12)))
	    ((= h 12) (setq h 0))))
	(encode-time s m h d mon y)))))

(defun gtd-fix-dangling-time (time-string)
  "Add empty minutes to time specified by only the hour, eg. 6p."
  (let ((hr-regexp " \\([01]?[0-9]\\) ?+[ap]"))
    (if (string-match hr-regexp time-string)
      (let* ((n (match-string 1 time-string))
	     (hr (concat n ":00")))
	(setq time-string
	  (replace-regexp-in-string
	    (match-string 1 time-string) hr time-string)))))
  time-string)

(defun gtd-parse-time-period (time-string)
  "Checks for am/pm in TIME-STRING and removes it. Return LIST: (new-time-string pm-p)"
  (let ((pm-p nil)
	(case-fold-search t)
	(time-period-regexp "\\(a\\.?m\\.?\\|am\\|a\\b\\|p\\.?m\\.?\\|pm\\|p\\b\\)"))
    (if (string-match time-period-regexp time-string)
      (let ((matched-str (match-string 1 time-string)))
	(if (eq t (compare-strings matched-str  0 1 "p" 0 1 t))
	  (setq pm-p t))
	(setq time-string (trim-string (replace-regexp-in-string time-period-regexp "" time-string)))))
    (list time-string pm-p)))


(defvar gtd-time-lang-defs
  '(("today"     . #'(lambda () (format-time-string "%b %d %Y" (current-time))))
    ("tomorrow"  . #'(lambda () (format-time-string "%b %d %Y" (time-add (current-time) (days-to-time 1)))))
    ("yesterday" . #'(lambda () (format-time-string "%b %d %Y" (time-subtract (current-time) (days-to-time 1)))))
    ("two days"  . #'(lambda () (format-time-string "%b %d %Y" (time-add (current-time) (days-to-time 2)))))
    ("three days". #'(lambda () (format-time-string "%b %d %Y" (time-add (current-time) (days-to-time 3)))))
    ("next week" . #'(lambda () (format-time-string "%b %d %Y" (time-add (current-time) (days-to-time 7))))))
  "Map string/regexp defintions to a replacement string or a function that returns a string.")

(defvar gtd-stop-words '("a" "an" "and" "are" "as" "at" "be" "by" "for" "from" "has" "he" "in" "is" "it" "its" "of" "on" "that" "the" "to" "was" "were" "will" "with")
  "List of common stop words to remove from input.")

(defun gtd-time-natural-lang-replace (time-string)
  "Replace natural language terms with their standard time definitions."
  ;;remove stopwords
  (mapcar #'(lambda (word)
	      (setq time-string
		(replace-regexp-in-string (concat "\\b\\(" word "\\)\\b") "" time-string)))
    gtd-stop-words)
  ;;replace time words
  (dolist (new-def gtd-time-lang-defs)
    (if (string-match (car new-def) time-string)
      (let ((replacement (eval (cdr new-def)))) ;some elisp weirdness when storing functions
	(setq time-string
	  (replace-regexp-in-string (car new-def) (if (functionp replacement)
						    (funcall replacement)
						    replacement)
	    time-string)))))
  ;;squeeze whitespace
  (replace-regexp-in-string " +" " " time-string))

;;
;; OSX Calendar App
;;

(defvar gtd-calendar-script-osx "\
%s
set isRunning to false
tell application \"System Events\"
  if (exists process \"Calendar\") then
    set isRunning to true
  end if
end tell
tell application \"System Events\" to tell application \"Calendar\"
  run
  delay 1
  tell (first calendar whose name is \"%s\")
    make new event at end of events with properties %s
  end tell
end tell
if not isRunning then
  tell application \"Calendar\" to quit
end if")

(defun format-date-vars (start end)
  (format "set startDate to date \"%s\"\nset endDate to date \"%s\"" start end))

(defun format-osx-event-script (calendar title start end &optional all-day-p description location)
  (format gtd-calendar-script-osx
    (format-date-vars start end)
    calendar
    (format-event-props title all-day-p description location)))

(defun format-alist-to-obj (alist)
  (let (outstr)
    (dolist (prop alist)
      (setq outstr (concat outstr ", " (car prop) ":" (cdr prop))))
    (setq outstr (replace-regexp-in-string "^, " "" outstr))
    (if outstr
      (concat "{" outstr "}"))))

(defun format-event-props (title &optional all-day-p description location)
  (let ((opts-alist (list
		      (cons "summary" (concat "\"" title "\""))
		      (cons "start date" "startDate") ;var name, requires date object creation
		      (cons "end date" "endDate"))))  ;var name
    (if all-day-p (add-to-list 'opts-alist (cons "allday event" "true") t))
    (if description (add-to-list 'opts-alist (cons "description" (concat "\"" description "\"")) t))
    (if location (add-to-list 'opts-alist (cons "location" (concat "\"" location "\"")) t))
    (format-alist-to-obj opts-alist)))
