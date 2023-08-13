;; gtd-mode.el
;; Emacs minor-mode for Getting Things Done.
;;
;; Copyright (C) 2014 Billy Lamberta
;;
;; Author: Billy Lamberta <b@lamberta.org>
;; Created: Apr 2014
;; Updated: Dec 2022
;; Keywords: todo, gtd
;; $Revision: 0.2 $
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
;;
;;
;; Description:
;;
;; `gtd-mode' is a minor mode that adds commands to quickly navigate
;; multiple files and the tags within. Search, go to, and move blocks
;; of text from one file to the next, automatically positioning it
;; beneath a selected destination context. This mode is a naive
;; interpretation of the Getting Things Done methodology for organization
;; using plain text files and simple tag markers for contexts.
;;
;;
;; Usage:
;;
;; Turn on the minor mode for a file using `M-x gtd-mode'. This mode
;; automatically enables `outline-minor-mode' but does not depend on it.
;;
;; `gtd-mode' works on "blocks of text", meaning lines of text
;; that are separated from one another by blank lines. Keeping
;; spaces between sections and entries will make it easier to move
;; them around later.
;;
;; As of now, the big commands are `gtd-goto' and `gtd-send', bound to
;; `C-c g' and `C-c m', respectively. Quickly go between files and tag
;; sections. Moving takes the current text block at your cursor, or
;; the highlighted region, and moves it to another section tag---even
;; in different files. gtd-mode will recognize any files set in
;; `gtd-files-alist' and will automatically use any of tags contained within.
;;
;; Setting `gtd-default-file' and `gtd-default-tag' to your favorites
;; allows you to quickly punch through the minibuffer prompts.
;;
;; Tags are, by default, prefixed with '@' and stand by themselves
;; at the beginning of a line. This format can be changed by setting
;; the variables `gtd-tag-format' and `gtd-tag-regexp'.
;;
;; To customize color highlights, use the `gtd-tag-face'.
;;
;;
;; Setup:
;;
;; To set up and auto-load this minor mode for the specified files,
;; your `.emacs' file could look something like this:
;;
;; (autoload 'gtd-mode "gtd-mode" "Minor mode for Getting Things Done." t)
;;
;; (setq
;;   gtd-file-alist
;;   '(("inbox" . "~/doc/inbox.txt")
;;     ("projects" . "~/doc/projects.md"))
;;   gtd-default-file "todo"
;;   gtd-default-tag "home"
;;   gtd-trash "~/doc/todo/.trash")
;;
;; (add-hook 'find-file-hook
;;   #'(lambda ()
;;       "Auto-enable gtd-mode for files in `gtd-file-alist'."
;;       (if (member (buffer-file-name (current-buffer))
;;             (mapcar #'expand-file-name (mapcar #'cdr gtd-file-alist)))
;;        (gtd-mode 1))))
;;
;;
;; An example of a gtd text file using context tags could look like:
;;
;; @waiting
;;
;; That expense report signature I gave to Steve-o,
;; maybe he sent it to Mary.
;;
;; Finish writing this example so I can upload the file.
;;
;; @books
;;
;; Getting Things Done by David Allen
;;
;; Gravity's Rainbow by Thomas Pynchon
;;
;; etc.
;;
;; Terms and concepts for this GTD implementation:
;; * file - A collection of one or more {lists}. Sometimes referred to as a {list} itself.
;; * list - A collection of {entries}, classified by a {label}, located in a {file}.
;; * entry - A single block of text contained in {list}. Entries are separated by a newline.
;; * label - Generically denotes a {list} section within a {file}. Format: '@listname'
;; * context - The {label} name for {lists} contained in the [next-actions] {file}.
;; * project - The {label} name for {lists} contained in the [projects] {file}.
;; * tag - Metadata associated with an {entry}, such as a timestamp. Format: '<tag data>'
;;
;; TODO:
;; * The goal is easy collection and one-click processing.
;; * actions moved out of projects have project tag appended (add tag but check for existing. need tag data format?)
;; * jump to next-entry in file (more about selecting entry under empty line)
;; * resolve name confusion in source: label-tags => contexts
;; * context aware menu options: action/project/reference/defer/etc
(require 'cl-lib)
(require 'outline)

(defgroup gtd nil
  "Support for Getting Things Done."
  :prefix "gtd-"
  :group 'editing)

(defcustom gtd-file-alist nil
  "Collection of shortnames and their corresponding file path."
  :type '(alist :key-type 'string :value-type 'file)
  :group 'gtd)

(defcustom gtd-archive-file-alist nil
  "Collection of shortnames and their corresponding file path to send entries
   for archival reference."
  :type '(alist :key-type 'string :value-type 'file)
  :group 'gtd)

(defcustom gtd-view-file-alist nil
  "Files to open when `gtd-toggle-view' is called. A file-path and integer
   argument to pass to `split-window'."
  :type '(alist :key-type 'file :value-type 'integer)
  :group 'gtd)

(defcustom gtd-default-file nil
  "Provide default file value to speed up minibuffer selection."
  :type 'string
  :group 'gtd)

(defcustom gtd-default-tag nil
  "Provide default tag value to speed up minibuffer selection."
  :type 'string
  :group 'gtd)

(defcustom gtd-default-font-size 0
  "Provide default relative font size. 0 is normal, 1 increase, -1 decrease, etc."
  :type 'integer
  :group 'gtd)

(defcustom gtd-default-mail nil
  "Mail address to send entries."
  :type 'string
  :group 'gtd)

(defcustom gtd-trash nil
  "Path to file where trashed items are sent. If file doesn't exist, just delete
   the item."
  :type 'file
  :group 'gtd)

(defcustom gtd-tag-format "@%s"
  "Format string to write the tag out."
  :type 'string
  :group 'gtd)

(defcustom gtd-tag-regexp "^@\\([[:alnum:]-_+]+\\)$"
  "Regex to select and capture the normalized tag.
   Allowed characters here may need accounting for in `gtd-escape-tag'."
  :type 'regexp
  :group 'gtd)

(defcustom gtd-hashtag-regexp "\\B#\\([[:alnum:]-_+]+\\)"
  "Regex to select and capture the normalized hashtag.
   Allowed characters here may need accounting for in `gtd-escape-tag'."
  :type 'regexp
  :group 'gtd)

;;
;; COLOR HIGHLIGHTS
;;

(defface gtd-tag-face '(
  (((class color) (background dark))  (:foreground "#DFAF8F")) ;zenburn-orange
  (((class color) (background light)) (:foreground "Red3"))
  (t (:bold t :italic t)))
  "gtd-mode face used for context labels."
  :group 'gtd)

(defface gtd-hashtag-face '(
  (((class color) (background dark))  (:foreground "#E0CF9F")) ;zenburn-yellow-1
  (((class color) (background light)) (:foreground "LightGoldenrod4"))
  (t (:bold t :italic t)))
  "gtd-mode face used for hashtags."
  :group 'gtd)

(defface gtd-subdue-face '(
  (((class color) (background dark))  (:foreground "#5F7F5F")) ;zenburn-green-1
  (((class color) (background light)) (:foreground "gray40"))
  (t (:bold nil :italic nil)))
  "gtd-mode face used for subdued text, links, and formatting characters."
  :group 'gtd)

(defface gtd-header-1-face '(
  (((class color) (background dark))  (:foreground "#8CD0D3")) ;zenburn-blue
  (((class color) (background light)) (:foreground "black"))
  (t (:bold nil :italic nil)))
  "gtd-mode face used for header one tags."
  :group 'gtd)

(defface gtd-header-2-face '(
  (((class color) (background dark))  (:foreground "#6CA0A3")) ;zenburn-blue-2
  (((class color) (background light)) (:foreground "black"))
  (t (:bold nil :italic nil)))
  "gtd-mode face used for header 2 tags."
  :group 'gtd)

(defface gtd-url-face '(
  (((class color) (background dark))  (:foreground "#9C6363")) ;zenburn-red-3
  (((class color) (background light)) (:foreground "black"))
  (t (:bold nil :italic nil)))
  "gtd-mode face used for URLs."
  :group 'gtd)
;;
;; UTILS
;;

(defun flatten-list (lst)
  "Return the leaf elements of tree LST merged in a non-hierarchical list."
  (cond
    ((null lst) nil)
    ((atom lst) (list lst))
    (t (append (flatten-list (car lst)) (flatten-list (cdr lst))))))

(defun assoc-all (key alist &optional append-p)
  "Return a list of all matching ALIST entries for KEY."
  (let ((entry-list (list)))
    (dolist (entry-pair alist)
      (if (equal key (car entry-pair))
        (add-to-list 'entry-list entry-pair append-p)))
    entry-list))

(defun rassoc-all (val alist &optional append-p)
  "Return a list of all matching ALIST entries for VAL."
  (let ((entry-list (list)))
    (dolist (entry-pair alist)
      (if (equal val (cdr entry-pair))
        (add-to-list 'entry-list entry-pair append-p)))
    entry-list))

(defun trim-string (str)
  "Trims leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'" str)
    (setq str (replace-match "" t t str)))
  str)

(defun gtd-squeeze-blank-lines ()
  "Replace multiple blank lines with a single one within the entire buffer."
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "\\(^\\s-*$\\)\n" nil t)
    (replace-match "\n")
    (forward-char 1)))

(defun gtd-timestamp ()
  "Insert the current time and date."
  (interactive)
  (let ((date-format "%l:%M%P %b %e %Y %a"))
    (insert (format "<%s>" (replace-regexp-in-string "[ ]+" " "
                             (replace-regexp-in-string "^[ ]" ""
                               (format-time-string date-format (current-time))))))))

(defun make-prompt (label &optional default-choice)
  "Create a prompt to use in the minibuffer."
  (if default-choice
    (format "%s [%s] " label default-choice)
    (format "%s " label)))

(defun gtd-read-string (prompt &optional default)
  "Prompt user for string. Does not accept an empty value."
  (let (str
        (prompt (make-prompt prompt default)))
    (while (null (valid-entry-p str))
      (setq str (read-string prompt nil nil default)))
    str))

(defun gtd-completing-read (itemlist &optional prompt default-val)
  "Read a menu selection from the minibuffer using ALIST for options.
   Return an ALIST of all key matches. Use `ido-completing-read' if it's
   available."
  (setq prompt (make-prompt (or prompt "Select:") default-val))
  (let* ((choices (all-completions "" itemlist))
         (completing-read (if (fboundp #'ido-completing-read) #'ido-completing-read #'completing-read))
         (entry (funcall completing-read prompt choices nil t nil nil default-val)))
    (if (member entry itemlist)
      entry
      nil)))

(defun completing-read-alist (alist &optional prompt default-val)
  "Read a menu selection from the minibuffer using ALIST for options.
   Return an ALIST of all key matches. Use `ido-completing-read' if it's
   available."
  (setq prompt (make-prompt (or prompt "Select:") default-val))
  (let* ((choices (all-completions "" alist))
         (completing-read (if (fboundp #'ido-completing-read) #'ido-completing-read #'completing-read))
         (entry (funcall completing-read prompt choices nil t nil nil default-val)))
    (if (valid-entry-p entry alist)
      (assoc-all entry alist))))

(defun valid-entry-p (entry &optional alist)
  "Test if ENTRY is a non-empty string. If passed ALIST, test if ENTRY is a
   valid key."
  (if (and (stringp entry) (not (string= entry "")))
    (if alist
      (if (assoc entry alist) t nil)
      t)))

(defun file-entry-p (entry)
  "Test if the ENTRY cons cell appears in `gtd-file-alist'."
  (if entry
    (let ((file-entry (assoc (car entry) gtd-file-alist)))
      (and file-entry (equal entry file-entry)))))

(defun distinct-alist (alist)
  "In case a menu ALIST contains duplicate keys, generate a distinct set."
  (let ((new-alist (list)))
    (dolist (pair alist)
      (let ((name (if (file-entry-p pair)
                    (format "%s" (car pair))
                    (format "%s-%s" (car (rassoc (cdr pair) gtd-file-alist)) (car pair)))))
        (cl-pushnew (cons name (cdr pair)) new-alist)))
    new-alist))

(defun gtd-escape-tag (tag)
  (let ((escaped-str tag))
    (dolist (ch '("+" "-"))
      (setq escaped-str (replace-regexp-in-string ch (concat "\\\\" ch) escaped-str)))
    escaped-str))

;;
;; FILES
;;

(defun with-file (filename callback &optional keep-open-p)
  "Open FILENAME in a new buffer where the CALLBACK function is executed.
   CALLBACK is passed a single STRING argument if an error occured, or NIL if
   ok. If not already open, a file buffer is closed when done unless
   KEEP-OPEN-P is T."
  (let ((filepath (expand-file-name filename)))
    (if (not (file-exists-p filepath))
      (funcall callback (format "Unable to read file: %s" filename))
      (let* ((filebuffer (get-file-buffer filepath))
             (is-open-p (not (null filebuffer))))
        (if (null filebuffer)
          (setq filebuffer (find-file-noselect filepath)))
        (with-current-buffer filebuffer
          (funcall callback nil)
          (if (and (not is-open-p) (not keep-open-p))
            (kill-buffer)))))))

(defun gtd-with-temp-file (filename callback)
  "Open FILENAME into a new temp buffer where the CALLBACK function is executed.
   Used for processing the contents of a file, not visiting and editing it."
  (let ((filepath (expand-file-name filename)))
    (if (not (file-exists-p filepath))
      (funcall callback (format "Unable to read file: %s" filename))
      (with-temp-buffer
        (insert-file-contents filepath)
        (funcall callback nil)))))

(defun gtd-select-file (file-alist &optional prompt default-val)
  "Select a file from FILE-ALIST and return its full path.
   Pass a string to customize PROMPT."
  (let* ((file-selected (cl-first (completing-read-alist file-alist prompt default-val)))
         (filepath (if file-selected (expand-file-name (cdr file-selected)))))
    (cl-assert file-selected)
    (cl-assert (file-writable-p filepath))
    filepath))

(defun file-touch (filename)
  "Use the touch shell command to create an empty file."
  (setq filename (expand-file-name filename))
  (if (and (not (file-exists-p filename)) (file-writable-p filename))
    (progn (call-process "touch" nil nil nil filename) t)
    nil))

(defun gtd-concatenate-file (dest-file src-file &optional clear-src-file-p)
  "Append the contents of SRC-FILE to the end of DEST-FILE. If CLEAR-SRC-FILE-P
   is T, clear SRC-FILE contents but keep the file. Return T on success,
   otherwise NIL. This function is useful for merging inbox files."
  (let (success-p)
    (with-current-buffer (find-file-noselect dest-file)
      (goto-char (point-max))
      (insert (eof-pad))
      ;;leave cursor at insertion point
      (save-excursion
        ;;on success, second list item is length of data inserted
        (setq success-p (> (cl-second (insert-file-contents src-file)) 0)))
      (delete-trailing-whitespace))
    ;;empty contents of src-file
    (if (and success-p clear-src-file-p)
      (with-current-buffer (find-file-noselect src-file)
        (call-interactively 'mark-whole-buffer)
        (kill-region (region-beginning) (region-end))
        (save-buffer)
        (kill-buffer)))
    success-p))

;;
;; TAGS
;;

(defun tag-list-from-file (filename &optional alist-p)
  "Return a LIST of tags contained within FILENAME, ignoring
   duplicates. If ALIST-P is T, create an alist using the tag
   as key and FILENAME for each value."
  (let ((tag-list (list)))
    (gtd-with-temp-file filename
      #'(lambda (err-msg)
          (if err-msg
            (error err-msg)
            (save-excursion
              (goto-char (point-min))
              ;;match each tag. ignores duplicates--not sure why?
              (while (re-search-forward gtd-tag-regexp nil t)
                (let ((tagname (match-string-no-properties 0)))
                  ;;normalize
                  (if (string-match gtd-tag-regexp tagname)
                    (setq tagname (match-string 1 tagname)))
                  (add-to-list 'tag-list (if alist-p
                                           (cons tagname filename)
                                           tagname) t)))))))
    tag-list))

(defun file-contains-tag-p (filename tag)
  "Return T or NIL if FILENAME contains TAG."
  (if (member-ignore-case tag (tag-list-from-file filename)) t nil))


(defun make-tag-file-alist ()
  "Return an ALIST of all tags and their corresponding files."
  (let ((tag-alist (list)))
    (dolist (file-pair gtd-file-alist)
      (let* ((filename (cdr file-pair))
             (file-tag-alist (mapcar #'(lambda (tag) (cons tag filename))
                               (tag-list-from-file filename))))
      (setq tag-alist (append tag-alist file-tag-alist))))
    tag-alist))


(defun file-alist-for-tag-alist (tag-alist)
  "Given a TAG-ALIST containing a cons of a tag name and file path,
   return a file alist with the files they appear in."
  (let ((file-alist (list))
        (tag (car (cl-first tag-alist))))
    (dolist (tag-pair tag-alist)
      (unless (string= tag (car tag-pair))
        (error "Does not support mixed tag names."))
      (let* ((filepath (cdr tag-pair))
             (shortname (car (rassoc filepath gtd-file-alist))))
        (unless (assoc shortname file-alist)
          (cl-pushnew (cons shortname filepath) file-alist))))
    file-alist))


(defun select-tag (tag &optional tag-alist)
  "Searches for TAG and returns a cons containing the tag name and
   its file. If tag appears in multiple files, prompt for clarification.
   If the tag is not found, return NIL."
  (if (null tag-alist) (setq tag-alist (make-tag-file-alist)))
  (let ((tag-alist-selected (assoc-all tag tag-alist)))
    (cond
      ((= (length tag-alist-selected) 0)
        nil)
      ((= (length tag-alist-selected) 1)
        (cl-first tag-alist-selected))
      (t
        ;;use default file if in the tag list, otherwise use first
        (let* ((default-file (if (rassoc (cdr (assoc gtd-default-file gtd-file-alist)) tag-alist-selected)
                               gtd-default-file
                               (car (rassoc (cdr (cl-first tag-alist-selected)) gtd-file-alist))))
               (prompt (format "Tag: %s, Which file?" tag))
               (file-alist (file-alist-for-tag-alist tag-alist-selected))
               (file-alist-selected (completing-read-alist file-alist prompt default-file)))
          (if file-alist-selected
            (rassoc (cdr (cl-first file-alist-selected)) tag-alist-selected)))))))


(defun read-tag-from-minibuffer (&optional prompt tag-alist)
  "Return a cons cell containing the tag name and its file.
   If tag appears in multiple files, prompt for clarification.
   Use `gtd-default-tag' and `gtd-default-file' if they are set.
   Optionally, set the PROMPT as a string. Pass TAG-ALIST to
   avoid generating new tag list."
  (if (null prompt) (setq prompt "Tag:"))
  (if (null tag-alist) (setq tag-alist (make-tag-file-alist)))
  ;;returns a list of tags in case of multiple occurances
  ;;if only 1, use it, otherwise prompt for clarification
  (let* ((default-tag (if (assoc gtd-default-tag tag-alist)
                        gtd-default-tag
                        (car (cl-first tag-alist))))
         (tag-alist-selected (completing-read-alist tag-alist prompt default-tag)))
    (if tag-alist-selected
      (if (= (length tag-alist-selected) 1)
        (cl-first tag-alist-selected)
        (select-tag (car (cl-first tag-alist-selected)) tag-alist-selected)))))

;;
;; SELECTION
;;

(defun mark-current-block ()
  "Marks the current text block region situated between empty lines."
  (interactive)
  (let (p1 (re-empty-lines "\n[ \t]*\n"))
    (if (re-search-backward re-empty-lines nil "move")
      (re-search-forward re-empty-lines))
    (setq p1 (point))
    (if (re-search-forward re-empty-lines nil "move")
      (re-search-backward re-empty-lines))
    (set-mark p1)))

(defun gtd-entry-at-point ()
  "Returns the current marked region or paragraph as a STRING.
   Removes leading characters defined in `gtd-entry-prefix'."
  (if (and (not (use-region-p))
           (not (string= (thing-at-point 'line) "\n"))) ;no empty line
    (mark-current-block))
  (if (use-region-p)
    (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
      ;;remove leading empty line
      (setq text (replace-regexp-in-string "\\`[ \t\n]*" "" text))
      text)))


;;
;; INSERTION
;;

(defun eof-pad ()
  "Return a STRING with end-of-file whitespace needed for adding a new entry
   in the current buffer. Deletes trailing whitespace if needed."
  (save-excursion
    (goto-char (point-max))
    (let ((char (char-before)))
      (cond
        ((null char) "")
        ((not (string= (string char) "\n"))
          (format "\n\n"))
        (t
          (delete-trailing-whitespace)
          (format "\n"))))))


(defun insert-string-at-tag (text tag)
  "Insert the TEXT string beneath the given TAG within the current buffer.
   Return the buffer position where the text starts insertion."
  (cl-assert (stringp text))
  (cl-assert (stringp tag))
  (let (pos (tag-marker (format gtd-tag-format (gtd-escape-tag tag))))
    (save-excursion
      (goto-char (point-min))
      ;;position at the end of the tag marker
      (if (re-search-forward tag-marker nil t)
        (progn
          ;;determines how an entry is formatted
          (insert "\n\n")
          (setq pos (point))
          (insert text)
          pos)
        nil))))


(defun gtd-send (&optional tag-alist goto-p)
  "Send current entry to the prompted tag in its associated file.
   By default, sending an entry doesn't follow it. If GOTO-P is T,
   open up the buffer and view at the insertion point."
  (interactive)
  (let ((text (gtd-entry-at-point)))
    (if text
      (let* ((action (if goto-p "Move" "Send"))
             (tag-pair (read-tag-from-minibuffer (concat action " to tag:") tag-alist))
             (filepath (if tag-pair (expand-file-name (cdr tag-pair)))))
        (cl-assert tag-pair)
        (when (and filepath (file-writable-p filepath))
          ;;kill selection
          (kill-region (region-beginning) (region-end))
          (delete-blank-lines)
          ;;insert text under label
          (with-file filepath
            #'(lambda (err-msg)
                (if err-msg
                  (error err-msg)
                  (let ((pos (insert-string-at-tag text (car tag-pair))))
                    (if (null pos)
                      (error "Unable to insert at tag.")
                      (progn
                        (save-buffer)
                        (when goto-p
                          (switch-to-buffer (current-buffer))
                          (goto-char pos)))))))
            goto-p))))))


(defun gtd-move (&optional tag-alist)
  "Move the current entry to the prompted tag and open the file."
  (interactive)
  (gtd-send tag-alist t))

(defun gtd-send-to-label-in-file (filename &optional goto-p fullpath-p)
  "Send the current entry to a context label in FILENAME, which is set
   in `gtd-file-alist'. The default is to use the shortname for the file
   unless FULLPATH-P is T. Open the file buffer if GOTO-P is T."
  (let* ((filepath (if fullpath-p
                     (cdr (rassoc filename gtd-file-alist))
                     (cdr (assoc filename gtd-file-alist))))
         (tag-alist (tag-list-from-file filepath t)))
    (cl-assert filepath nil "File not found: %s" filename)
    (cl-assert tag-alist nil "No tags found in file: %s" filename)
    (if filepath
      (gtd-send tag-alist goto-p)
      nil)))


(defun gtd-append-entry-to-file-alist (file-alist &optional prompt goto-p)
  "Append the current entry to the end of the selected file from FILE-ALIST.
   If GOTO-P is T, open the buffer and view at the insertion point."
  (let ((entry (gtd-entry-at-point))
        (filepath (gtd-select-file file-alist prompt)))
    (when (and entry filepath)
      ;;kill entry in old file
      (kill-region (region-beginning) (region-end))
      (delete-blank-lines)
      ;;insert entry in new file
      (with-file filepath
        #'(lambda (err-msg)
            (if err-msg (error err-msg))
            (goto-char (point-max))
            (insert (eof-pad))
            (save-excursion
              (insert entry))
            (save-buffer)
            (if goto-p
              (switch-to-buffer (current-buffer))))
        goto-p))))


(defun gtd-delete ()
  "Delete the selected entry from the current buffer.
   If `gtd-trash' is set, append the entry to that file."
  (interactive)
  (let ((entry (gtd-entry-at-point)))
    (if (not entry)
      (progn
        (message "No entry selected.")
        nil)
      (let ((sent-to-trash-p nil)
            (filepath (if (stringp gtd-trash)
                        (expand-file-name gtd-trash))))
        ;;if trash file set, send entry there
        (when (and filepath (file-writable-p filepath))
          (file-touch filepath)
          (with-file filepath
            #'(lambda (err-msg)
                (if err-msg
                  (message err-msg)
                  (progn
                    (goto-char (point-max))
                    (insert (eof-pad) entry)
                    (save-buffer)
                    (setq sent-to-trash-p t))))))
        ;;delete entry in current buffer
        (kill-region (region-beginning) (region-end))
        (delete-blank-lines)
        (if sent-to-trash-p
          (message "Entry sent to trash.")
          (message "Entry deleted."))
        t))))

;;
;; MOVEMENT
;;

(defun gtd-goto-file (&optional filename shortname-p)
  "Open the file buffer designated by FILENAME, or prompt if not given.
   Use shortname for file lookup if SHORTNAME-P is T."
  (interactive)
  (let (filepath)
    (cond
      ((and (not (null filename)) shortname-p)
        (setq filepath (cdr (assoc filename gtd-file-alist))))
      ((not (null filename))
        (setq filepath filename))
      (t
        (let ((file-alist (completing-read-alist gtd-file-alist "Go to file:" gtd-default-file)))
          (setq filepath (cdr (cl-first file-alist))))))
    (if filepath
      (setq filepath (expand-file-name filepath)))
    (if (file-exists-p filepath)
      (progn (switch-to-buffer (find-file-noselect filepath)) t)
      (progn (message "Unable to read file: %s" filepath) nil))))


(defun gtd-goto-tag (&optional tag tag-alist)
  "Open the file buffer and position where TAG is located."
  (interactive)
  (if (null tag-alist) (setq tag-alist (make-tag-file-alist)))
  (let (tag-pair err)
    (setq tag-pair (if tag
                     (select-tag tag tag-alist)
                     (read-tag-from-minibuffer "Go to tag:" tag-alist)))
    (if (null tag-pair)
      (setq err (format "Unable to locate tag: %s" tag))
      (let ((keep-open-p t))
        (with-file (cdr tag-pair)
          #'(lambda (err-msg)
              (if err-msg
                (setq err err-msg)
                (let ((tag-marker (format gtd-tag-format (gtd-escape-tag (car tag-pair)))))
                  (goto-char (point-min))
                  (if (not (re-search-forward tag-marker nil t))
                    (setq err (format "Unable to find tag %s in file %s" (car tag-pair) (cdr tag-pair)))
                    (progn
                      (beginning-of-line)
                      (switch-to-buffer (current-buffer))))))) keep-open-p)))
    (if err
      (progn (message err) nil)
      t)))

(defun gtd-goto-project ()
  "Go to a context label in the projects file."
  (interactive)
  (let ((filename (cdr (assoc "projects" gtd-file-alist))))
    (gtd-goto-tag nil (tag-list-from-file filename t))))

(defun gtd-goto (&optional selector)
  "Quickly open a file or context label."
  (interactive)
  (let* (selected-alist selected-pair
         (tag-alist (make-tag-file-alist))
         (combined-alist (append gtd-file-alist tag-alist)))
    (setq selected-alist (if selector
                           (assoc-all selector combined-alist)
                           (completing-read-alist combined-alist "Go to:")))
    (cond
      ((null selected-alist)
        (message "Nothing selected."))
      ((= (length selected-alist) 1)
        (setq selected-pair (cl-first selected-alist)))
      (t
        (let* ((menu-alist (distinct-alist selected-alist))
                (menu-selected-pair (cl-first (completing-read-alist menu-alist "File or tag?")))
                (idx (cl-position menu-selected-pair menu-alist :test #'equal)))
          (setq selected-pair (elt selected-alist idx)))))
    (if (null selected-pair)
      nil
      (if (file-entry-p selected-pair)
        (gtd-goto-file (cdr selected-pair))
        (gtd-goto-tag (car selected-pair) tag-alist)))))

;;
;; FILE SPECIFIC !!!
;; Convenience shortcuts for now, until I figure out a better config abstraction.
;;

(defun gtd-send-to-archive ()
  "Send the current entry to a file selected from `gtd-archive-file-alist'."
  (interactive)
  (gtd-append-entry-to-file-alist gtd-archive-file-alist "Send to archive:"))

(defun gtd-move-to-archive ()
  "Move the current entry to a file selected from `gtd-archive-file-alist'.
   Open the file buffer at the insertion point."
  (interactive)
  (gtd-append-entry-to-file-alist gtd-archive-file-alist "Move to archive:" t))

(defun gtd-send-to-project ()
  "Send the current entry to the project label in the [projects] file."
  (interactive)
  (gtd-send-to-label-in-file "projects"))

(defun gtd-move-to-project ()
  "Move the current entry to the project label in the [projects] file."
  (interactive)
  (gtd-send-to-label-in-file "projects" t))

(defun gtd-send-to-action ()
  "Send the current entry to the context label in the [next-actions] file."
  (interactive)
  (gtd-send-to-label-in-file "next-actions"))

(defun gtd-move-to-action ()
  "Move the current entry to the context label in the [next-actions] file."
  (interactive)
  (gtd-send-to-label-in-file "next-actions" t))


;;
;; MISC.
;;

(defun gtd-insert-timestamp-and-newline ()
  "Inserts a timestamp and newline after an entry. Tries not to break timestamp
   on separate lines."
  (interactive)
  (let* ((timestamp (with-temp-buffer
                      (gtd-timestamp)
                      (buffer-string)))
         (total-width (+ (current-column) (length timestamp))))
    (if (< total-width fill-column)
      (insert " " timestamp "\n")
      (insert "\n" timestamp "\n")))
  (delete-trailing-whitespace)
  (save-buffer))

(defun gtd-mail-compose (beg end)
  "Edit message to send `gtd-default-mail'. Insert a selected region in body."
  (interactive (if (use-region-p)
                 (list (region-beginning) (region-end))
                 (list nil nil)))
  (let ((region-string (if (and beg end)
                         (buffer-substring-no-properties beg end))))
    (mail 'new gtd-default-mail)
    (call-interactively 'mail-text)
    (if region-string
      (insert region-string))))

(defun gtd-map-open-buffers (fn &rest args)
  "Execute function within any open buffers from `gtd-file-alist'."
  (save-current-buffer
    (dolist (rec gtd-file-alist)
      (let ((buf (get-file-buffer (cdr rec))))
        (when buf
          (set-buffer buf)
          (apply fn args))))))

(defun gtd-text-scale-increase (&optional n)
  "Increase the height of the default face in all open GTD buffers."
  (interactive)
  (unless n (setq n 1))
  (gtd-map-open-buffers 'text-scale-increase n))

(defun gtd-text-scale-decrease (&optional n)
  "Decrease the height of the default face in all open GTD buffers."
  (interactive)
  (unless n (setq n 1))
  (gtd-map-open-buffers 'text-scale-decrease n))

(defun gtd-text-scale-reset ()
  "Reset the height of the default face in all open GTD buffers."
  (interactive)
  (gtd-map-open-buffers 'text-scale-set 0))

(defun gtd-save-buffers ()
  "Save any modified buffers in `gtd-file-alist'."
  (interactive)
  ;get list of modified gtd buffers, filter nils
  (let ((modified-bufs
          (delq nil (mapcar
                      #'(lambda (fp-cell)
                          (let ((buf (get-file-buffer (cdr fp-cell))))
                            (if (and buf (buffer-modified-p buf))
                              buf)))
                      gtd-file-alist))))
    (save-some-buffers t #'(lambda ()
                             (member (current-buffer) modified-bufs)))))

(defvar gtd-view-open-p nil)
(defvar gtd-win-config nil)
(defvar gtd-open-file-buffer nil)

(defun gtd-view-toggle (&optional horizontal-p use-current-window-p split-file-alist filename map-fn)
  "Open/close a split-pane view of files in SPLIT_FILE_ALIST or
   `gtd-view-file-alist'. Default to vertical split, if HORIZONTAL-P is T, split
   windows horizontally. Will remove open windows unless USE-CURRENT-WINDOW-P is
   T. If FILENAME is set, split and open the file in the big window."
  (interactive)
  (if (null split-file-alist)
    (setq split-file-alist gtd-view-file-alist))
  (if (null split-file-alist)
    (message "Must provide SPLIT-FILE-ALIST arg or set `gtd-view-file-alist' to view.")
    (if gtd-view-open-p
      (progn
        ;;close em down
        (setq gtd-view-open-p nil)
        ;;reset old window config
        (if gtd-win-config
          (set-window-configuration gtd-win-config))
        ;;kill buffers
        (dolist (fp split-file-alist)
          (kill-buffer (get-file-buffer (car fp))))
        (when gtd-open-file-buffer
          (kill-buffer gtd-open-file-buffer)
          (setq gtd-open-file-buffer nil)))
      (progn
        ;; open buffers and configure view
        (setq
          gtd-view-open-p t
          gtd-win-config (current-window-configuration))
        ;;open panes in fresh configuration vs within existing window
        (unless use-current-window-p
          (delete-other-windows))
        ;;split and open specified file in big window then jump back
        (if filename
          (if (not (file-exists-p filename))
            (message "Unable to find file: %s" filename)
            (progn
              (if horizontal-p
                (split-window-vertically)
                (split-window-horizontally))
              (other-window 1)
              (find-file filename)
              (setq gtd-open-file-buffer (get-file-buffer filename))
              (previous-multiframe-window))))
        ;;open each file in new window, then return to first
        (let ((win (get-buffer-window))
              (default-split-size (if horizontal-p
                                    (/ (window-total-width) (length split-file-alist))
                                    (/ (window-total-height) (length split-file-alist)))))
          (dolist (fp split-file-alist)
            (find-file (car fp))
            (when (cdr fp)
              (if horizontal-p
                (split-window-right default-split-size) ;no custom value now
                (split-window-below (cdr fp))) ;size specified in setup
              (other-window 1)))
          (select-window win))
        ;; adjust font size (if set)
        (if (/= gtd-default-font-size 0)
          (gtd-text-scale-increase gtd-default-font-size))
        ;; if passed function, apply to all open buffers
        (if map-fn
          (gtd-map-open-buffers map-fn))))))

;;
;; KEYBINDINGS
;;

(defun gtd-display-keybindings ()
  "Print a quick summary of the GTD keybindings. Bound to: 'C-c ?'"
  (interactive)
  (message "Commands: {g}oto, {m}ove, {s}end, {a|A}rchive, {n|N}ext-action, {p|P}roject, {d}elete, {c}alendar, {T}imestamp"))

(defvar gtd-mode-map (make-sparse-keymap))
(define-key gtd-mode-map (kbd "C-x C-s") 'gtd-save-buffers)
(define-key gtd-mode-map (kbd "C-c g") 'gtd-goto)
(define-key gtd-mode-map (kbd "C-c m") 'gtd-move)
(define-key gtd-mode-map (kbd "C-c s") 'gtd-send)
(define-key gtd-mode-map (kbd "C-c a") 'gtd-send-to-archive)
(define-key gtd-mode-map (kbd "C-c A") 'gtd-move-to-archive)
(define-key gtd-mode-map (kbd "C-c n") 'gtd-send-to-action)
(define-key gtd-mode-map (kbd "C-c N") 'gtd-move-to-action)
(define-key gtd-mode-map (kbd "C-c p") 'gtd-send-to-project)
(define-key gtd-mode-map (kbd "C-c P") 'gtd-move-to-project)
(define-key gtd-mode-map (kbd "C-c <backspace>") 'gtd-delete)
(define-key gtd-mode-map (kbd "C-c T") 'gtd-timestamp)
(define-key gtd-mode-map (kbd "C-<return>") 'gtd-insert-timestamp-and-newline)
(define-key gtd-mode-map (kbd "C-c ?") 'gtd-display-keybindings)
;standard text resize changes all open gtd buffers
(define-key gtd-mode-map (kbd "s-=") 'gtd-text-scale-increase)
(define-key gtd-mode-map (kbd "s--") 'gtd-text-scale-decrease)
(define-key gtd-mode-map (kbd "s-0") 'gtd-text-scale-reset)
;;shift-resize only changes current buffer
(define-key gtd-mode-map (kbd "s-+") 'text-scale-increase) ;s-S-=
(define-key gtd-mode-map (kbd "s-_") 'text-scale-decrease) ;s-S--
(define-key gtd-mode-map (kbd "s-)") #'(lambda () (interactive) (text-scale-set 0))) ;s-S-0
;;REMOVE: (define-key gtd-mode-map (kbd "C-c c") 'gtd-calendar-add-event)

;;
;; GO!
;;

(defvar gtd-tag-prefix nil)
(defvar gtd-tag-face 'gtd-tag-face "gtd-mode face used for context tags.")
(defvar gtd-hashtag-face 'gtd-hashtag-face "gtd-mode face used for hashtags.")
(defvar gtd-subdue-face 'gtd-subdue-face "gtd-mode face used for subdued text.")
(defvar gtd-header-1-face 'gtd-header-1-face "gtd-mode face used for header one tags.")
(defvar gtd-header-2-face 'gtd-header-2-face "gtd-mode face used for header two tags.")
(defvar gtd-url-face 'gtd-url-face "gtd-mode face used for URLs.")

(define-minor-mode gtd-mode
  "Commands for Getting Things Done."
  :lighter " gtd"
  :keymap gtd-mode-map
  :after-hook (run-after-hooks)
  (font-lock-mode 1)
  (outline-minor-mode 1))

(defun run-after-hooks ()
  ;;highlighting
  (let ((link-def "\\(\\[\\)\\(.*\\)\\(\\]\\[.*\\]\\)") ;3 groups
        (link-desc "^\\[[A-Za-z0-9]*\\]:[[:space:]]?.*$"))
    (font-lock-add-keywords nil
      (list
        (list gtd-tag-regexp 0 gtd-tag-face t)
        (list gtd-hashtag-regexp 0 gtd-hashtag-face t)
        '("<.*>" 0 gtd-subdue-face t)          ;bracket comments
        '("//.*$" 0 gtd-subdue-face t)         ;line comments
        '("^#\\s-+.*$" 0 gtd-header-1-face t)  ;markdown-style headers
        '("^##\\s-+.*$" 0 gtd-header-2-face t)
        '("-\\*-.*-\\*-" 0 gtd-subdue-face t)  ;emacs file vars
        '("\\(https?://[^[:space:]\r\n>]+\\).*?$" 1 gtd-url-face t) ;url
        (list link-def 1 gtd-subdue-face t)    ;front bracket
        (list link-def 3 gtd-subdue-face t)    ;end of tags
        (list link-desc 0 gtd-subdue-face t))))
  ;;imenu
  (add-to-list 'imenu-generic-expression (list nil gtd-tag-regexp 1))
  (setq-local imenu-auto-rescan t)
  (imenu-add-to-menubar "Tags")
  ;;outline
  (setq gtd-tag-prefix (replace-regexp-in-string "%s.*" "" gtd-tag-format))
  ;;the shorter the match, the higher up the hierarchy. "^@\\|[*]+\\s-"
  ;(setq-local outline-regexp (format "^%s\\|%s\\s-" gtd-tag-prefix outline-regexp))
  (setq-local outline-regexp "^@.+\\|^#+.+")) ;tag and markdown header

(eval-after-load 'outline-minor
  (progn
    (defun gtd-outline-cycle-header ()
      "If cursor is on a header, toggle visibility of corresponding children.
       Otherwise, insert a tab using `indent-relative'."
      (interactive)
      (if (not (outline-on-heading-p t))
        (indent-relative)
        (if (outline-invisible-p (line-end-position))
          (outline-show-subtree)
          (outline-hide-subtree))))
    (defun gtd-outline-cycle-all ()
      "Toggle visibility of all headers in file. If any are hidden, show all."
      (interactive)
      (save-excursion
        (goto-char (point-min))
        (let ((found-invisible-heading-p nil))
          (while (and (outline-next-heading) (null found-invisible-heading-p))
            (if (outline-invisible-p (line-end-position))
              (setq found-invisible-heading-p t)))
          (if found-invisible-heading-p
            (outline-show-all)
            (outline-hide-body)))))
    ;; key bindings
    (define-key gtd-mode-map (kbd "<tab>") 'gtd-outline-cycle-header)
    (define-key gtd-mode-map (kbd "S-<tab>") 'gtd-outline-cycle-all)))

(provide 'gtd-mode)
