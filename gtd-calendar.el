;; gtd-calendar.el
;; Calendar integration for `gtd-mode', a minor-mode for Getting Things Done.
;;
;; Copyright (C) 2014 Billy Lamberta
;;
;; Author: Billy Lamberta <b@lamberta.org>
;; Created: Apr 2014
;; Updated: Apr 2014
;; Keywords: todo, gtd, calendar
;; $Revision: 0.1 $
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(defcustom gtd-calendars nil
  "List of available calendars. Used for a calendar selection choices, the first being the default."
  :type 'list
  :group 'gtd-calendar)

(defcustom gtd-calendar-action nil
  "Function to call when adding a calendar event. This is system dependant."
  :type 'function
  :group 'gtd-calendar)


;;
;; UI FLOW
;;

(defun gtd-calendar-add-event (&optional cal-name title start end all-day-p description location)
  "Add an event to the calendar."
  (interactive)
  (let ((return-message
          (catch 'exit-calendar-add-event
            (progn
              (let* ((first-input (gtd-read-string "Add Event:"))
                     (first-input-parts (split-string first-input ";"))
                     (multi-input-p (> (length first-input-parts) 1))) ;contains time/title

                ;;grab title
                (setq title (trim-string (if multi-input-p
                                           (second first-input-parts)
                                           first-input)))

                ;;grab times, will confirm
                (let ((event-times (if multi-input-p
                                     (gtd-read-event-time (trim-string (first first-input-parts)))
                                     (gtd-read-event-time))))
                  (if (null event-times)
                    (throw 'exit-calendar-add-event "Unable to read event time.")
                    (setq
                      start     (nth 0 event-times)
                      end       (nth 1 event-times)
                      all-day-p (nth 2 event-times)))))

              ;;grab calendar
              (setq cal-name (gtd-completing-read gtd-calendars "Calendar" (car gtd-calendars)))

              ;;validate args
              (when (called-interactively-p 'interactive)
                (let ((entry (gtd-entry-at-point)))
                  (if (and entry (yes-or-no-p "Include entry in event description? "))
                    (setq description entry))))

              ;;dispatch
              (cond
                (gtd-calendar-action
                  (funcall #'gtd-calendar-action cal-name title start end all-day-p description location))
                ((eq system-type 'darwin)
                  (gtd-osx-default-calendar cal-name title start end all-day-p description location))
                (t
                  (throw 'exit-calendar-add-event "Not implemented on this system."))))
            (progn
              ;;success! or at least no errors thrown
              (gtd-cal-on-add-event-success title cal-name start end)))))
    (message return-message)))


(defun gtd-cal-on-add-event-success (title cal-name start end)
  "This function is run when an event is successfully added to the calendar.
   Returns a string message that is displayed in the minibuffer.
   Possibly add other system popup messages as well here."
  (let ((start-str (format-time-string "%b %d %Y %I:%M %p" start))
        (end-str (format-time-string "%b %d %Y %I:%M %p" end))
        (title-short (if (> (length title) 25)
                       (concat (trim-string (substring title 0 22)) "...")
                       title)))
    (format "Added event '%s' to %s, %s" title-short cal-name start-str)))


;;
;; EVENT TIME PROMPT
;;

(defun gtd-read-event-time (&optional time-input)
  "Prompt user for an event's start time and end time.
   If provided, the string TIME-INPUT will get parsed
   instead of prompting the user for the initial input.
   Return list: (start-time end-time all-day-p)"
  (assert (if time-input (stringp time-input) t))
  (cl-labels
    ((multi-prompt (prompt-list &optional callback last-try-p)
       ;;Reads user input, using a first try prompt and subsequent tries prompt.
       ;;Apply CALLBACK function to values to test input.
       (let* ((prompt (if last-try-p (second prompt-list) (first prompt-list)))
              (val (gtd-read-string prompt)))
         (if (functionp callback) (setq val (funcall callback val)))
         (if (or val last-try-p)
           val
           (multi-prompt prompt-list callback t))))

      (parse-input-as-time (val)
        ;;For string VAL, if range return 2 times in list, or 1 time if not.
        (assert (stringp val) t "parse-input-as-time val: %s" val)
        (let ((time-range (gtd-parse-time-range val)))
          (if (null time-range)
            (gtd-encode-time-string val)
            time-range)))

      (has-time-p (time)
        ;;Return T if the encoded/decoded TIME is set
        ;;non-zero for seconds, minutes, and hour."
        (if (= (length time) 2)
          (setq time (decode-time time)))
        (assert (>= (length time) 6))
        (or (/= 0 (nth 0 time)) (/= 0 (nth 1 time)) (/= 0 (nth 2 time))))

      (check-all-day (time-1 &optional time-2)
        ;;Test if the time arguments have sec/min/hr.
        (if (has-time-p time-1)
          nil
          (if (and time-2 (has-time-p time-2))
            nil
            (yes-or-no-p "All day event? ")))))

    ;;Using function arg or user prompt, parse input string to time values.
    ;;Will be an encoded time, or if a range, a list of 2 encoded times.
    (let (start-time end-time all-day-p time-parsed return-list)
      ;;if thrown a nil, keep trying
      (while (null
               (catch 'retry-time-input
                 (progn
                   (setq time-parsed (if time-input
                                       (parse-input-as-time time-input)
                                       (multi-prompt '("When?" "Sorry, didn't get that. When?") #'parse-input-as-time)))
                   ;;reset in case we go around again
                   (setq time-input nil)
                   (cond
                     ;;got range
                     ((and (= (length (flatten-list time-parsed)) 4) (every #'numberp (flatten-list time-parsed)))
                       (setq
                         start-time (first time-parsed)
                         end-time (second time-parsed))
                       (setq all-day-p (check-all-day start-time end-time)))

                     ;;got start time only
                     ((and (= (length time-parsed) 2) (every #'numberp time-parsed))
                       (setq start-time time-parsed)
                       (setq all-day-p (check-all-day start-time))
                       ;;get end time
                       (unless all-day-p
                         (setq time-parsed (multi-prompt '("End time:" "Sorry, didn't get that. When?") #'gtd-encode-time-string))
                         (when time-parsed
                           (setq end-time time-parsed)
                           (setq all-day-p (check-all-day end-time)))))

                     ;;shouldn't get here, but whatever it is, it ain't right
                     (time-parsed
                       (setq time-parsed nil)))

                   ;;sanity check
                   (if (and end-time (time-less-p end-time start-time))
                     (if (yes-or-no-p (gtd-make-time-confirm-prompt "It ends before it begins! Re-enter?" start-time end-time all-day-p))
                       (throw 'retry-time-input nil)))
                   ;;confirm if we have something
                   (unless (yes-or-no-p (gtd-make-time-confirm-prompt "Correct?" start-time end-time all-day-p))
                     (throw 'retry-time-input nil))
                   ;;success
                   (setq return-list (list start-time end-time all-day-p)))
                 t)))
      return-list)))


(defun gtd-make-time-confirm-prompt (prompt start-time end-time all-day-p)
  "Generate a confirmation prompt for event time details."
  (let (args (day-single "%a, %b %d %Y") (day-time "%a, %b %d %Y %I:%M %p"))
    (if start-time
      (setq args (format-time-string
                   (if all-day-p day-single day-time)
                   start-time)))
    (if end-time
      (setq args (concat args " - " (format-time-string
                                      (if all-day-p day-single day-time)
                                      end-time))))
    (if all-day-p
      (setq args (concat args ", all-day")))
    ;;and out
    (format "%s [%s]" prompt args)))


;;
;; PARSE DATETIME
;;

(defun gtd-encode-time-string (time-string)
  "Parse the TIME-STRING and apply reasonable defaults.
   Return the encoded time, or NIL if unable to parse."
  (assert (and (stringp time-string) (not (string= time-string ""))))
  ;;replace keywords and regex groups
  (setq time-string (gtd-replace-time-keywords time-string))
  ;;trim and squeeze whitespace
  (setq time-string (trim-string (replace-regexp-in-string "\\s-+" " " time-string)))
  ;;split time digits from time-period
  (let* (time-list
         (time-period (gtd-split-time-period
                        (gtd-pad-minutes time-string)))
         (pm-p (eq 'PM (third time-period))))
    (condition-case nil
      ;;hopefully by now the time string is ready for component parsing
      (setq time-list (butlast (parse-time-string (first time-period)) 3))
      ;;report error and the function returns nil.
      ;;will hold off on any automatic corrections.
      (parse-error
        (message (format "gtd-encode-time-string: Unable to parse '%s'" (first time-period)))))
    ;;if time-list is full of nils, we got a problem
    (if (not (delq t (mapcar #'null time-list)))
      nil
      ;;work our way through the time list elements,
      ;;prefer ones explicitly set, but if not there, be smart about a default
      (let* ((cur-time-list (butlast (parse-time-string (current-time-string)) 3))
             (s (or (nth 0 time-list) 0))
             (m (or (nth 1 time-list) 0))
             (h (or (nth 2 time-list) 0))
             (d (or (nth 3 time-list) (nth 3 cur-time-list)))
              ;default to this month, unless day is less than today, then next month
              (mon (or (nth 4 time-list)
                     (let ((cur-d (nth 3 cur-time-list))
                           (cur-mon (nth 4 cur-time-list)))
                       (if (< d cur-d)
                         (if (= cur-mon 12) 1 (1+ cur-mon))
                         cur-mon))))
              ;default to this year, unless mon is less that this one, then next year
              (y (or (nth 5 time-list)
                   (let ((cur-mon (nth 4 cur-time-list))
                         (cur-y (nth 5 cur-time-list)))
                     (if (< mon cur-mon)
                       (1+ cur-y)
                       cur-y)))))
        ;;account for pm
        (if (and pm-p (< h 12))
          (setq h (+ h 12)))
        (encode-time s m h d mon y)))))


;; Step for parsing time range from input string:
;; 1. If there is a range operator ('-' or 'to') in the input string,
;; 2. Greedy match chars around that, creating a date section and time section.
;; 3. Split the time section on the range operator, creating two time parts.
;; 4. Normalize time parts, checking for it's own time period (am/pm).
;; 5. If a time part is missing a time period, determine one for it.
;; 6. Concat date section with each time part and return them both.
(defun gtd-parse-time-range (time-string)
  "Return a list containing two timestamps parsed from a date
   and time range contained in TIME-STRING."
  (cl-flet ((pad-time (time-list)
              ;;Return the given TIME-LIST with the first element correctly padded.
              (cons (gtd-pad-minutes (first time-list)) (cdr time-list)))

            (concat-time-period (time-list &optional alt-time-list)
              ;;Return a string concatenating the time and time-period of TIME-LIST.
              ;;Use the time-period from ALT-TIME-LIST if provided.
              (let ((period-sym (cond
                                  ((and alt-time-list (third alt-time-list))
                                    (third alt-time-list))
                                  ((third time-list)
                                    (third time-list)))))
                (if period-sym
                  (format "%s %s" (first time-list) (symbol-name period-sym))))))
    ;;greedy match times surrounding the range operator
    (assert (stringp time-string))
    (setq time-string (gtd-replace-time-keywords time-string t))
    (let ((time-regexp "[^ ]+ ?+[apm\\.]?* ?+[-\\|to]\\b.*"))
      (when (string-match time-regexp time-string)
               ;;grab time chunk and split in two
        (let* ((datetime-list (split-string (match-string 0 time-string) "-\\|to"))
                ;;formalize the time string blob into time and time-period parts 
               (time-1-list (pad-time (gtd-split-time-period (first datetime-list))))
               (time-2-list (pad-time (gtd-split-time-period (second datetime-list))))
                ;;if there's an associated time period, create the canonical timestamp. or nil.
               (time-1-str (concat-time-period time-1-list))
               (time-2-str (concat-time-period time-2-list)))
          ;;if no associated time period, determine it from the other time part
          (cond
            ((and (null time-1-str) (null time-2-str))
              ;;got nothing. use system default (probably AM)
              (setq
                time-1-str (first time-1-list)
                time-2-str (first time-2-list)))
            ((and time-1-str (null time-2-str))
              (setq time-2-str
                (concat-time-period time-2-list time-1-list)))
            ((and time-2-str (null time-1-str))
              (setq time-1-str
                (concat-time-period time-1-list time-2-list))))
          ;;grab date chunk, encode, and re-format
          (let ((date-part (trim-string (replace-regexp-in-string time-regexp "" time-string))))
            (if (not (string= date-part ""))
              (setq date-part (format-time-string "%b %d %Y"
                                (gtd-encode-time-string date-part))))
            ;;concat and return the two datetimes, using the full canonical format
            (list
              (gtd-encode-time-string (concat date-part " " time-1-str))
              (gtd-encode-time-string (concat date-part " " time-2-str)))))))))


;;this is a tricky function, especially when dates are included in the input.
(defun gtd-pad-minutes (time-string)
  "Add empty minutes to time specified by only the hour, eg. 6p."
  (assert (stringp time-string))
  (cond
    ;;ignore if hh:mm already exists
    ((string-match "[0-2]?[0-9]:[0-9][0-9]" time-string)
      nil)
    ;;ignore a trailing solo number preceded by letter (month date)
    ((string-match "[A-Za-z]\\s-*[0-2]?[0-9]$" (trim-string time-string))
      nil)
    ;;match other trailing solo number
    ((string-match "\\s-*[0-2]?[0-9]$" (trim-string time-string))
      (setq time-string (concat time-string ":00")))
    ;;match numbers with time period and insert
    ((string-match "\\([0-2]?[0-9]\\) ?+\\([ap].*\\)" time-string)
      (let* ((n (match-string 1 time-string))
             (hr (concat n ":00" (match-string 2 time-string))))
        (setq time-string (replace-regexp-in-string
                            (match-string 0 time-string) hr time-string)))))
  time-string)


(defun gtd-split-time-period (time-string &optional return-original-p)
  "Separate TIME-STRING into datetime/time-period components. Returns a
   three element list: (time-string time-period period-symbol), where
   period-symbol is either 'AM or 'PM. If RETURN-ORIGINAL-P is T,
   return TIME-STRING unmodified."
  (assert (stringp time-string))
  (let (time-period-str time-period-sym
        (time-period-regexp "\\(a\\.?m\\.?\\|am\\|a\\b\\|p\\.?m\\.?\\|pm\\|p\\b\\)")
        (case-fold-search t))
    (when (string-match time-period-regexp time-string)
      (setq time-period-str (match-string 1 time-string))
      (setq time-period-sym
        (if (eq t (compare-strings time-period-str  0 1 "p" 0 1 t))
          'PM
          'AM))
      (unless return-original-p
        (setq time-string
          (replace-regexp-in-string time-period-regexp "" time-string))))
    (unless return-original-p
      (setq time-string (trim-string time-string)))
    (list time-string time-period-str time-period-sym)))


(defun gtd-next-day-distance (day)
  "The number of days to the next occuring DAY name, excluding today.
   Returns 1-7, or NIL if DAY isn't matched."
  (let ((today-val (string-to-number (format-time-string "%w" (current-time))))
        (day-val (cond
                   ((string-match "^su[n]?$\\|^sunday$" day) 0)
                   ((string-match "^m$\\|^mo[n]?$\\|^monday$" day) 1)
                   ((string-match "^tu$\\|^tue[s]?$\\|^tuesday$" day) 2)
                   ((string-match "^w$\\|^we[d]?$\\|^wednesday$" day) 3)
                   ((string-match "^th[u]?$\\|^thur[s]?$\\|^thursday$" day) 4)
                   ((string-match "^f$\\|^fr[i]?$\\|^friday$" day) 5)
                   ((string-match "^sa[t]?$\\|^saturday$" day) 6))))
    (if day-val
      (let ((dist (- day-val today-val)))
        (if (< dist 1)
          (+ dist 7)
          dist)))))


(defvar gtd-time-keywords
  '(("today"     . #'(lambda () (format-time-string "%b %d %Y" (current-time))))
    ("tomorrow"  . #'(lambda () (format-time-string "%b %d %Y" (time-add (current-time) (days-to-time 1)))))
    ("yesterday" . #'(lambda () (format-time-string "%b %d %Y" (time-subtract (current-time) (days-to-time 1)))))
    ("two days"  . #'(lambda () (format-time-string "%b %d %Y" (time-add (current-time) (days-to-time 2)))))
    ("three days". #'(lambda () (format-time-string "%b %d %Y" (time-add (current-time) (days-to-time 3)))))
    ("next week" . #'(lambda () (format-time-string "%b %d %Y" (time-add (current-time) (days-to-time 7)))))
    ("\\+\\([0-9]+\\)d" . #'(lambda (n) (format-time-string "%b %d %Y" (time-add (current-time) (days-to-time (string-to-number n))))))
    ("\\-\\([0-9]+\\)d" . #'(lambda (n) (format-time-string "%b %d %Y" (time-subtract (current-time) (days-to-time (string-to-number n))))))
    ("\\+\\([0-9]+\\)w" . #'(lambda (n) (format-time-string "%b %d %Y" (time-add (current-time) (days-to-time (* 7 (string-to-number n)))))))
    ("\\-\\([0-9]+\\)w" . #'(lambda (n) (format-time-string "%b %d %Y" (time-subtract (current-time) (days-to-time (* 7 (string-to-number n)))))))
    ("\\b\\+\\([a-zA-Z]+\\)" . #'(lambda (day) (let ((n (gtd-next-day-distance day)))
                                                 (if n (format-time-string "%b %d %Y" (time-add (current-time) (days-to-time n)))))))
    ("\\b\\+\\+\\([a-zA-Z]+\\)" . #'(lambda (day) (let ((n (gtd-next-day-distance day)))
                                                    (if n (format-time-string "%b %d %Y" (time-add (current-time) (days-to-time (+ 7 n))))))))
    ("\\b\\+\\+\\+\\([a-zA-Z]+\\)" . #'(lambda (day) (let ((n (gtd-next-day-distance day)))
                                                    (if n (format-time-string "%b %d %Y" (time-add (current-time) (days-to-time (+ 14 n)))))))))
  "Map string/regexp definitions to a replacement string or a function that returns a string.
   Matched regexp groups are passed to the replacement function as arguments.")


(defvar gtd-stop-words '("a" "an" "and" "are" "as" "at" "be" "by" "for" "from" "has" "he" "in" "is" "it" "its" "of" "on" "that" "the" "to" "was" "were" "will" "with")
  "List of common stop words to remove from input.")


(defun gtd-replace-time-keywords (time-string &optional keep-stopwords-p)
  "Replace keywords in TIME-STRING with their standard time definitions in `gtd-time-keywords'.
   And words contained in `gtd-stop-words' are filtered out."
  ;;replace time words
  (dolist (new-def gtd-time-keywords)
    (if (string-match (car new-def) time-string)
      (let ((replacement (eval (cdr new-def))) ;some elisp weirdness for stored functions
            (match-group-list (list))
            (i 1))
        ;;collect match groups
        (while (match-string i time-string)
          (add-to-list 'match-group-list (match-string i time-string))
          (setq i (1+ i)))
        ;;substitute word or apply function
        (let ((replace-text (if (functionp replacement)
                              (apply replacement match-group-list)
                              replacement)))
          (when replace-text
            (assert (stringp replace-text))
            (setq time-string (replace-regexp-in-string (car new-def) replace-text time-string)))))))
  ;;remove stopwords
  (unless keep-stopwords-p
    (mapcar #'(lambda (word)
                (setq time-string
                  (replace-regexp-in-string (concat "\\b\\(" word "\\)\\b") "" time-string)))
      gtd-stop-words))
  ;;return
  time-string)


;;
;; OSX CALENDAR.APP
;;

(defun gtd-osx-default-calendar (cal-name title start-encoded &optional end-encoded all-day-p desc loc)
  "Add an event to Calendar.app."
  (let* ((time-format "%b %d, %Y %I:%M %p")
         (start (format-time-string time-format start-encoded))
         (end (if end-encoded (format-time-string time-format end-encoded)))
         (cal-script (gtd-format-osx-event-script cal-name title start end all-day-p desc loc)))
    (assert (stringp start))
    (assert (stringp cal-script))
    (start-process "osx-calendar-add-event" nil "/usr/bin/osascript" "-e" cal-script))
  t)


(defun gtd-format-osx-event-script (calendar title start end &optional all-day-p description location)
  "Returns the AppleScript program `gtd-calendar-script-osx' with variable substitution."
  (format gtd-calendar-script-osx
    (gtd-format-date-vars start end)
    calendar
    (gtd-format-event-props title start end all-day-p description location)))


(defun gtd-format-date-vars (start end)
  "Return an AppleScript statement for setting variables for the START and END date."
  (assert (stringp start))
  (let ((out-string (format "set startDate to date \"%s\"" start)))
    (when end
      (assert (stringp end))
      (setq out-string (format "%s\nset endDate to date \"%s\"" out-string end)))
    out-string))


(defun gtd-format-event-props (title &optional start end all-day-p description location)
  "Generates the AppleScript statements needed to substitute for the given function arguments."
  (let ((opts-alist (list (cons "summary" (concat "\"" title "\"")))))
    (if start (add-to-list 'opts-alist (cons "start date" "startDate") t)) ;var name, requires date object creation
    (if end (add-to-list 'opts-alist (cons "end date" "endDate") t))       ;var name
    (if all-day-p (add-to-list 'opts-alist (cons "allday event" "true") t))
    (if description (add-to-list 'opts-alist (cons "description" (concat "\"" description "\"")) t))
    (if location (add-to-list 'opts-alist (cons "location" (concat "\"" location "\"")) t))
    (gtd-format-alist-to-obj opts-alist)))


(defun gtd-format-alist-to-obj (alist)
  "Iterates over ALIST properties and returns a string in AppleScript object format."
  (let (outstr)
    (dolist (prop alist)
      (setq outstr (concat outstr ", " (car prop) ":" (cdr prop))))
    (setq outstr (replace-regexp-in-string "^, " "" outstr))
    (if outstr
      (concat "{" outstr "}"))))


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
end if"
  "The AppleScript code that interacts with Calendar.app.")
