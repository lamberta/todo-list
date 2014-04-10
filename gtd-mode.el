;; gtd-mode.el
;; Emacs minor-mode for Getting Things Done.
;;
;; Copyright (C) 2014 Billy Lamberta
;;
;; Author: Billy Lamberta <b@lamberta.org>
;; Created: Apr 2014
;; Updated: Apr 2014
;; Keywords: todo, gtd
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
;; * Move task from beneath a project label to the next-actions list,
;;   automatically appending the project name as a tag to the moved task.
;; * jump to next-entry in file
;; * resolve name confusion: label-tags => contexts
;; * add applescript command for adding entry to calendar
;; * context aware menu options: action/project/reference/defer/etc

(defgroup gtd nil
  "Support for Getting Things Done."
  :prefix "gtd-"
  :group 'editing)

(defcustom gtd-file-alist nil
  "Collection of shortnames and their corresponding file path."
  :type '(alist :key-type 'string :value-type 'file)
  :group 'gtd)

(defcustom gtd-default-file nil
  "Provide default file value to speed up minibuffer selection."
  :type 'string
  :group 'gtd)

(defcustom gtd-default-tag nil
  "Provide default tag value to speed up minibuffer selection."
  :type 'string
  :group 'gtd)

(defcustom gtd-trash nil
  "Path to file where trashed items are sent. If file doesn't exist, just delete the item."
  :type 'file
  :group 'gtd)

(defcustom gtd-tag-format "@%s"
  "Format string to write the tag out."
  :type 'string
  :group 'gtd)

(defcustom gtd-tag-regexp "^@\\(\\w+\\)$"
  "Regex to select and capture the normalized tag."
  :type 'regexp
  :group 'gtd)

;;
;; COLOR HIGHLIGHTS
;;

(defface gtd-tag-face '(
  (((class color) (background dark))  (:foreground "IndianRed1"))
  (((class color) (background light)) (:foreground "Red3"))
  (t (:bold t :italic t)))
  "gtd-mode face used for context labels."
  :group 'gtd)

(defface gtd-subdue-face '(
  (((class color) (background dark))  (:foreground "gray40"))
  (((class color) (background light)) (:foreground "gray40"))
  (t (:bold nil :italic nil)))
  "gtd-mode face used for subdued text, links, and formatting characters."
  :group 'gtd)

;;
;; UTILS
;;

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

(defun completing-read-alist (alist &optional prompt default-val)
  "Read a menu selection from the minibuffer using ALIST for options."
  (setq prompt (make-prompt (or prompt "Select:") default-val))
  (let ((entry (completing-read prompt alist nil t nil nil default-val)))
    (if (valid-entry-p entry)
      (assoc-all entry alist))))

(defun valid-entry-p (entry &optional alist)
  "Test if ENTRY is a non-empty string. If passed ALIST, test if ENTRY is a valid key."
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
        (add-to-list 'new-alist (cons name (cdr pair)) t)))
    new-alist))

;;
;; FILES
;;

(defun with-file (filename callback &optional keep-open-p)
  "Open FILENAME in a new buffer where the CALLBACK function is executed.
   CALLBACK is passed a single STRING argument if an error occured, or NIL if ok.
   If not already open, a file buffer is closed when done unless KEEP-OPEN-P is T."
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

(defun with-temp-file (filename callback)
  "Open FILENAME into a new temp buffer where the CALLBACK function is executed.
   Used for processing the contents of a file, not visiting and editing it."
  (let ((filepath (expand-file-name filename)))
    (if (not (file-exists-p filepath))
      (funcall callback (format "Unable to read file: %s" filename))
      (with-temp-buffer
        (insert-file-contents filepath)
        (funcall callback nil)))))

(defun read-filename-from-minibuffer (&optional prompt)
  "Prompt user to select a gtd file contained in `gtd-file-alist'.
   Return the entry containing the shortname and file path.
   If `gtd-default-file' is set, use the as the prompt default."
  (unless (and (boundp 'gtd-file-alist) (> (length gtd-file-alist) 0))
    (error "No GTD files designated in GTD-FILE-ALIST."))
  (if (null prompt) (setq prompt "File:"))
  (let ((shortname (completing-read (make-prompt prompt gtd-default-file) gtd-file-alist nil t nil nil gtd-default-file)))
    (if (or (null shortname) (string= "" shortname))
      nil
      (assoc shortname gtd-file-alist))))

(defun file-touch (filename)
  "Use the touch shell command to create an empty file."
  (setq filename (expand-file-name filename))
  (if (and (not (file-exists-p filename)) (file-writable-p filename))
    (progn (call-process "touch" nil nil nil filename) t)
    nil))

;;
;; TAGS
;;

(defun tag-list-from-file (filename &optional alist-p)
  "Return a LIST of tags contained within FILENAME, ignoring
   duplicates. If ALIST-P is T, create an alist using the tag
   as key and FILENAME for each value."
  (let ((tag-list (list)))
    (with-temp-file filename
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
        (tag (car (first tag-alist))))
    (dolist (tag-pair tag-alist)
      (unless (string= tag (car tag-pair))
        (error "Does not support mixed tag names."))
      (let* ((filepath (cdr tag-pair))
             (shortname (car (rassoc filepath gtd-file-alist))))
        (unless (assoc shortname file-alist)
          (add-to-list 'file-alist (cons shortname filepath)))))
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
        (setq tag-pair (first tag-alist-selected)))
      (t
        (let* ((prompt (format "Tag: %s, Which file?" tag))
               (file-alist (file-alist-for-tag-alist tag-alist-selected))
               (file-alist-selected (completing-read-alist file-alist prompt gtd-default-file)))
          (if file-alist-selected
            (rassoc (cdr (first file-alist-selected)) tag-alist-selected)))))))

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
  (let ((tag-alist-selected (completing-read-alist tag-alist prompt gtd-default-tag)))
    (if tag-alist-selected
      (if (= (length tag-alist-selected) 1)
        (first tag-alist-selected)
        (select-tag (car (first tag-alist-selected)) tag-alist)))))

;;
;; SELECTION
;;

(defun mark-current-block ()
  "Marks the current text block region situated between empty lines."
  (interactive)
  (let (p1 p2)
    (progn
      (if (re-search-backward "\n[ \t]*\n" nil "move")
        (progn
          (re-search-forward "\n[ \t]*\n")
          (setq p1 (point)))
        (setq p1 (point)))
      (if (re-search-forward "\n[ \t]*\n" nil "move")
        (progn
          (re-search-backward "\n[ \t]*\n")
          (setq p2 (point)))
        (setq p2 (point))))
    (set-mark p1)))

(defun entry-at-point ()
  "Returns the current marked region or paragraph as a STRING.
   Removes leading characters defined in `gtd-entry-prefix'."
  (if (and (not (use-region-p))
           (not (string= (thing-at-point 'line) "\n"))) ;no empty line
    (mark-current-block))
  (if (use-region-p)
    (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
      ;;remove leading empty line
      (setq text (replace-regexp-in-string "\\`[ \t\n]*" "" text))
      ;;remove leading entry prefix. not needed?
      ;(setq text (replace-regexp-in-string (concat "^\\" gtd-entry-prefix) "" text))
      text)))

;;
;; INSERTION
;;

(defun eof-pad ()
  "Return a STRING with end-of-file whitespace needed for adding a new entry
   in the current buffer. Deletes trailing whitespace if needed."
  (save-excursion
    (end-of-buffer)
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
  (unless (and (stringp text) (stringp tag))
    (error "Invalid arguments."))
  (let (pos (tag-marker (format gtd-tag-format tag)))
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
  (let ((text (entry-at-point)))
    (if text
      (let* ((action (if goto-p "Move" "Send"))
	     (tag-pair (read-tag-from-minibuffer (concat action " to tag:") tag-alist))
             (filepath (if tag-pair
                         (expand-file-name (cdr tag-pair))
                         nil)))
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

(defun gtd-send-to-label-in-file (filename &optional goto-p fullpath-p)
  "Send the current entry to a context label in FILENAME, which is set
   in `gtd-file-alist'. The default is to use the shortname for the file
   unless FULLPATH-P is T. Open the file buffer if GOTO-P is T."
  (let ((filepath (if fullpath-p
		    (cdr (rassoc filename gtd-file-alist))
		    (cdr (assoc filename gtd-file-alist)))))
    (if filepath
      (gtd-send (tag-list-from-file filepath) goto-p)
      nil)))

(defun gtd-delete ()
  "Delete the selected entry from the current buffer.
   If `gtd-trash' is set, append the entry to that file."
  (interactive)
  (let ((entry (entry-at-point)))
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
                    (end-of-buffer)
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
          (setq filepath (cdr (first file-alist))))))
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
                (let ((tag-marker (format gtd-tag-format (car tag-pair))))
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
        (setq selected-pair (first selected-alist)))
      (t
        (let* ((menu-alist (distinct-alist selected-alist))
                (menu-selected-pair (first (completing-read-alist menu-alist "File or tag?")))
                (idx (position menu-selected-pair menu-alist :test #'equal)))
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

(defun gtd-send-to-project ()
  "Send the current entry to the project label in the [projects] file."
  (interactive)
  (gtd-send-to-label-in-file "projects"))

(defun gtd-move-to-project ()
  "Move the current entry to the project label in the [projects] file."
  (interactive)
  (gtd-send-to-label-in-file "projects" t))

(defun gtd-send-to-actions ()
  "Send the current entry to the context label in the [next-actions] file."
  (interactive)
  (gtd-send-to-label-in-file "next-actions"))

(defun gtd-move-to-actions ()
  "Move the current entry to the context label in the [next-actions] file."
  (interactive)
  (gtd-send-to-label-in-file "next-actions" t))

;;
;; KEYBINDINGS
;;

(defun gtd-display-keybindings ()
  (interactive)
  (message "Commands [C-c]: {g}oto, {m}ove, {s}end, {d}elete, {T}imestamp"))

(defvar gtd-mode-map (make-sparse-keymap))
(define-key gtd-mode-map (kbd "C-c g") 'gtd-goto)
(define-key gtd-mode-map (kbd "C-c m") 'gtd-move)
(define-key gtd-mode-map (kbd "C-c s") 'gtd-send)
(define-key gtd-mode-map (kbd "C-c d") 'gtd-delete)
(define-key gtd-mode-map (kbd "C-c <backspace>") 'gtd-delete)
(define-key gtd-mode-map (kbd "C-c T") 'gtd-timestamp)
(define-key gtd-mode-map (kbd "C-c ?") 'gtd-display-keybindings)

;;
;; GO!
;;

(defvar gtd-tag-prefix nil)
(defvar gtd-tag-face 'gtd-tag-face "gtd-mode face used for context tags.")
(defvar gtd-subdue-face 'gtd-subdue-face "gtd-mode face used for subdued text.")

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
        '("<.*>" 0 gtd-subdue-face t)
        (list link-def 1 gtd-subdue-face t) ;front bracket
        (list link-def 3 gtd-subdue-face t) ;end of tags
        (list link-desc 0 gtd-subdue-face t))))
  ;;imenu
  (add-to-list 'imenu-generic-expression (list nil gtd-tag-regexp 1))
  (setq-local imenu-auto-rescan t)
  (imenu-add-to-menubar "Tags")
  ;;outline
  (setq gtd-tag-prefix (replace-regexp-in-string "%s.*" "" gtd-tag-format))
  (setq-local outline-regexp (format "%s\\|%s.+" outline-regexp gtd-tag-prefix)))

(eval-after-load 'outline-minor
  (progn
    (defun gtd-outline-cycle ()
      "If cursor is on a header, toggle visibility of corresponding children. Otherwise, insert a tab using `indent-relative'."
      (interactive)
      (if (not (outline-on-heading-p t))
        (indent-relative)
        (if (outline-invisible-p (line-end-position))
          (show-subtree)
          (hide-subtree))))
    (define-key gtd-mode-map (kbd "<tab>") 'gtd-outline-cycle)))

(provide 'gtd-mode)
