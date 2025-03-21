;; todo-list-mode.el
;; Emacs to set your goals while keeping up with life.
;;
;; Copyright (C) 2009 Billy Lamberta
;;
;; Author: Billy Lamberta
;; URL: https://github.com/lamberta/todo-list
;; Created: Jan 2009
;; Updated: Mar 2025
;; Keywords: todo, todo-list, gtd, outline
;; Version: 2.0
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
;; Description:
;;
;; `todo-list-mode' is a minor mode to quickly capture and navigate your
;; projects and goals labeled within plain text files. This is my simplified
;; interpretation of Getting Things Done (David Allen) after 10 years of use.
;; Here, the six horizons of focus are reduced to two file collections:
;; 1. active *projects* with attached tasks, and
;; 2. *goals* that set direction to generate projects.
;; Headings are available to group these further, as needed.
;;
;; Setup:
;;
;; It's important to quickly capture ideas for processing later. Add this to
;; your `.emacs' with an easy keybinding to open your files:
;;
;; (autoload 'todo-list-mode "todo-list-mode" "Set your goals." t)
;;
;; (setq todo-list-files
;;   '(:left "~/todo/projects.txt"
;;     :right "~/todo/goals.txt"
;;     :hide-right t))  ; collapse outline
;;
;; (autoload 'todo-list-toggle "todo-list-mode" "Quickly view your goals." t)
;; (global-set-key (kbd "s-t") 'todo-list-toggle)  ; macos
;;
;; Usage:
;;
;; In your text files, create project labels at the beginning of a line and
;; prefix with '@'.
;; This mode uses `outline-mode' to collapse and expand entries. Headings start
;; with '#' and can separate multiple projects, such as:
;;
;; @read-gravitys-rainbow
;; Buy book
;; Find a dictionary
;;
;; # Work projects
;;
;; @promo-project
;; Sort paperclips by weight

(require 'outline)

(defgroup todo-list nil
  "Customization group for `todo-list-mode'."
  :prefix "todo-list-"
  :group 'editing)

;; USER SETTINGS

(defcustom todo-list-files nil
  "Plist to set the file paths to open. Uses the following keys:
  - :left (string): File to open in the left buffer.
  - :right (string): File to open in the right buffer.
  - :hide-left (boolean): Whether to collapse the outline headers.
  - :hide-right (boolean): Whether to collapse the outline headers."
  :type '(plist :key-type (choice (const :left)
                                  (const :right)
                                  (const :hide-left)
                                  (const :hide-right))
                :value-type (choice (file :must-match t)
                                    boolean))
  :group 'todo-list)

(defcustom todo-list-font-size -2
  "Relative adjustment of font size across open buffers. [0, 1, -1, ...]"
  :type 'integer
  :group 'todo-list)

;; COLORS

(defface todo-list-project-face
  '((t (:foreground "#DFAF8F" :bold t)))  ; zenburn-orange
  "Face for project labels."
  :group 'todo-list)

(defface todo-list-heading-face
  '((t (:foreground "#8CD0D3")))  ; zenburn-blue
  "Face for headings between multiple projects."
  :group 'todo-list)

(defface todo-list-comment-face
  '((t (:foreground "#5F7F5F" :italic t)))  ; zenburn-green-1
  "Face for comments and subdued text."
  :group 'todo-list)

(defface todo-list-url-face
  '((t (:foreground "#9C6363")))  ; zenburn-red-3
  "Face for URLs."
  :group 'todo-list)

(defvar todo-list-font-lock-keywords
  '(("^@[[:graph:]]+" . 'todo-list-project-face)
    ("^#+ \\(.+\\)" . 'todo-list-heading-face)
    ("//.*$" . 'todo-list-comment-face)
    ("https?://[[:graph:]]+" 0 'todo-list-url-face t))
  "Syntax highlighting for `todo-list-mode'.")

;; FILE MANAGEMENT

(defun todo-list-get-files ()
  "Return a list of file paths from `todo-list-files'."
  (mapcar #'(lambda (key)
              (expand-file-name (plist-get todo-list-files key)))
          '(:left :right)))

(defun todo-list-open-files ()
  "Open files from `todo-list-files' in side-by-side windows."
  (if (not (and (plist-member todo-list-files :left)
                (plist-member todo-list-files :right)))
    (message "`todo-list-files' requires both `:left' and `:right' entries.")
    ;; Parse user settings from the plist
    (let ((left (expand-file-name (plist-get todo-list-files :left)))
          (right (expand-file-name (plist-get todo-list-files :right)))
          (hide-left-p (plist-get todo-list-files :hide-left))
          (hide-right-p (plist-get todo-list-files :hide-right)))
      (if (not (and (file-exists-p left) (file-exists-p right)))
        (message "Unable to read both file entries in `todo-list-files'.")
        (progn
          ;; Open the files in side-by-side windows
          (delete-other-windows)
          (find-file left)
          (todo-list-mode 1)
          (if hide-left-p
            (outline-hide-body))
          (outline-next-heading)  ; Start cursor at first entry for each file.
          (split-window-right)
          (find-file-other-window right)
          (todo-list-mode 1)
          (if hide-right-p
            (outline-hide-body))
          (outline-next-heading)
          (select-window (get-buffer-window (find-file-noselect left))))))))

(defvar todo-list-files-open-p nil
  "Flag to track if the todo-list files are currently open.")

(defvar todo-list-saved-win-config nil
  "Variable to store the previous window configuration.")

;;;###autoload
(defun todo-list-toggle ()
  "Open and close `todo-list-mode' while preserving window configuration."
  (interactive)
  (if todo-list-files-open-p
    (progn
      (setq todo-list-files-open-p nil)
      (when todo-list-saved-win-config
        (set-window-configuration todo-list-saved-win-config)
        (setq todo-list-saved-win-config nil)
        ;; remove buffers
        (dolist (file (todo-list-get-files))
          (kill-buffer (get-file-buffer file)))))
    (progn
      (setq todo-list-saved-win-config (current-window-configuration))
      (setq todo-list-files-open-p t)
      (todo-list-open-files)
      (todo-list-set-font-size))))

(defun todo-list-save-buffers ()
  "Save any modified buffers in files from `todo-list-files'."
  (interactive)
  (dolist (file (todo-list-get-files))
    (let ((buffer (find-buffer-visiting file)))
      (when (and buffer (buffer-modified-p buffer))
        (with-current-buffer buffer
          (save-buffer))))))

;; OUTLINE

(defun todo-list-current-heading-level ()
  "Return the outline heading level at point. [1, 2, nil]"
  (when (outline-on-heading-p t)
    (save-excursion
      (outline-back-to-heading)
      (if (looking-at outline-regexp)
          (let ((match-count (length (match-string 0))))
            ;; More characters matched means a deeper level.
            (cond
             ((= match-count 0) nil)
             ((= match-count 2) 1)
             ((> match-count 2) 2)))
        nil))))

(defun todo-list-toggle-heading ()
  "Toggle the visibility of the current heading."
  (interactive)
  (when-let ((level (todo-list-current-heading-level)))
    (save-excursion
      (outline-back-to-heading)
      (let ((invisible-p (outline-invisible-p (line-end-position))))
        (pcase level
          (1 (if invisible-p
               (outline-show-subtree)
               (outline-hide-leaves)))
          (2 (if invisible-p
               (outline-show-entry)
               (outline-hide-entry))))))))

(defun todo-list-toggle-headings ()
  "Toggle the visibility of all headings."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((invisible-p nil))
      (while (and (outline-next-heading) (not invisible-p))
        (if (outline-invisible-p (line-end-position))
          (setq invisible-p t)))
      (if invisible-p
        (outline-show-all)
        (outline-hide-body)))))

;; NAVIGATION

(defun todo-list-goto-section (move-fn)
  "Move to outline section based on MOVE-FN."
  (let (pos)
    (save-excursion
      (while (and (null pos) (funcall move-fn))
        (when-let ((level (todo-list-current-heading-level)))
          (if (= level 1)
            (setq pos (point))))))
    (if pos
      (goto-char pos))))

(defun todo-list-next-section ()
  "Move to next section."
  (interactive)
  (todo-list-goto-section #'outline-next-heading))

(defun todo-list-previous-section ()
  "Move to previous section."
  (interactive)
  (todo-list-goto-section #'outline-previous-heading))

(defun todo-list-collect-headings (buffers)
  "Collect all outline headings with position from BUFFERS."
  (let ((headings-alist '()))  ; (("name". (buf . pos)) ...)
    (save-excursion
      (dolist (buffer buffers)
        (with-current-buffer buffer
          (goto-char (point-min))
          (while (re-search-forward outline-regexp nil t)
            (let* ((pos (line-beginning-position))
                   (heading (buffer-substring-no-properties pos (line-end-position))))
              (add-to-list 'headings-alist
                (cons heading (cons buffer pos)) t))))))
    headings-alist))

(defun todo-list-find-entry ()
  "Find and jump to a `todo-list-mode' entry."
  (interactive)
  (let* ((buffers (mapcar #'get-file-buffer (todo-list-get-files)))
         (headings-alist (todo-list-collect-headings buffers))
         (choices (mapcar #'car headings-alist))
         (selection (completing-read "Todo-list entry: " choices)))
    (if-let ((entry (assoc selection headings-alist)))
      (progn
        (select-window (get-buffer-window (cadr entry)))
        (goto-char (cddr entry)))
      (message "Todo-list entry not found: %s" selection))))

;; ACCESSIBILITY

(defun todo-list-set-font-size ()
  "Set the font size according to `todo-list-font-size'."
  (dolist (file (todo-list-get-files))
    (let ((buffer (find-buffer-visiting file)))
      (when buffer
        (with-current-buffer buffer
          (text-scale-set todo-list-font-size))))))

(defun todo-list-increase-font-size ()
  "Increase the font size."
  (interactive)
  (setq todo-list-font-size (1+ todo-list-font-size))
  (todo-list-set-font-size))

(defun todo-list-decrease-font-size ()
  "Decrease the font size."
  (interactive)
  (setq todo-list-font-size (1- todo-list-font-size))
  (todo-list-set-font-size))

(defun todo-list-reset-font-size ()
  "Reset the font size."
  (interactive)
  (custom-reevaluate-setting 'todo-list-font-size)
  (todo-list-set-font-size))

;; KEYBINDINGS

(defvar todo-list-mode-map (make-sparse-keymap))
(define-key todo-list-mode-map (kbd "C-x C-s") 'todo-list-save-buffers)
(define-key todo-list-mode-map (kbd "<tab>") 'todo-list-toggle-heading)
(define-key todo-list-mode-map (kbd "S-<tab>") 'todo-list-toggle-headings)
(define-key todo-list-mode-map (kbd "C-c /") 'todo-list-find-entry)
;; Tip: Disable macOS keyboard shortcuts for ctrl-<direction>
(define-key todo-list-mode-map (kbd "C-<down>") 'outline-next-heading)
(define-key todo-list-mode-map (kbd "C-<up>") 'outline-previous-heading)
(define-key todo-list-mode-map (kbd "C-<right>") 'todo-list-next-section)
(define-key todo-list-mode-map (kbd "C-<left>") 'todo-list-previous-section)

(when (eq system-type 'darwin)
  (define-key todo-list-mode-map (kbd "s-=") 'todo-list-increase-font-size)
  (define-key todo-list-mode-map (kbd "s--") 'todo-list-decrease-font-size)
  (define-key todo-list-mode-map (kbd "s-0") 'todo-list-reset-font-size))

;; MODE

;;;###autoload
(define-minor-mode todo-list-mode
  "Set your goals while keeping up with life."
  :lighter " todo-list"
  :keymap todo-list-mode-map
  (outline-minor-mode 1)
  ; Define outline headings. More characters matched means a deeper nested level.
  (setq-local outline-regexp "^#[[:blank:]]?\\|^@[[:graph:]]+")
  (font-lock-add-keywords nil todo-list-font-lock-keywords)
  (font-lock-mode 1))

(provide 'todo-list-mode)
