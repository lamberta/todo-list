;;; todo-list-mode.el --- Life... But how to live it? -*- lexical-binding: t -*-
;; Emacs to set your goals while keeping up with life.
;;
;; Copyright (C) 2009 Billy Lamberta
;;
;; Author: Billy Lamberta
;; URL: https://github.com/lamberta/todo-list
;; Created: Jan 2009
;; Updated: Mar 2025
;; Keywords: convenience, outlines, org
;; Version: 2.0
;; Package-Requires: ((emacs "29.1"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; `todo-list-mode' is a minor mode that provides a two-panel view for managing
;; projects and goals using `org-mode'. This organization style was inspired by
;; the Getting Things Done methodology but reduced to two basic collections to
;; section off as you like:
;; 1. active *projects* with attached tasks, and
;; 2. *goals* that set direction to generate projects.
;;
;; Setup:
;;
;; Clone the project and add this to your `.emacs':
;;
;; (use-package todo-list-mode
;;   :load-path "~/src/todo-list"
;;   :after org
;;   :custom
;;   (todo-list-files
;;     '(:projects "~/org/projects.org"
;;       :goals "~/org/goals.org"))
;;   :bind (("C-c t t" . todo-list-toggle)
;;          ("C-c t f" . todo-list-entry-find))
;;   :bind (:map todo-list-mode-map
;;          ("C-c t p" . todo-list-entry-add-properties)))
;;
;; Usage:
;;
;; todo-list-toggle - Toggle todo-list view
;; todo-list-entry-find - Jump to any entry across both files
;; todo-list-entry-add-properties - Add a property drawer to the current heading

;;; Code:

(require 'org)
(require 'org-id)
(require 'cl-lib)

;;; Customization

(defgroup todo-list nil
  "Customization group for `todo-list-mode'."
  :prefix "todo-list-"
  :group 'org)

(defcustom todo-list-files nil
  "Configuration for project and goal files.
A plist with keys :projects and :goals pointing to org files."
  :type '(plist :key-type (choice (const :projects)
                                  (const :goals))
                :value-type (file :must-match t))
  :group 'todo-list)

(defcustom todo-list-font-size -1
  "Relative adjustment of font size for todo-list buffers."
  :type 'integer
  :group 'todo-list)

;;; Keymaps

(defvar todo-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-s") #'todo-list-save-buffers)
    (define-key map (kbd "C-<down>") #'org-next-visible-heading)
    (define-key map (kbd "C-<up>") #'org-previous-visible-heading)
    (define-key map (kbd "C-<right>") #'todo-list-next-section)
    (define-key map (kbd "C-<left>") #'todo-list-previous-section)

    ;; Font size adjustments for macOS
    (when (eq system-type 'darwin)
      (define-key map (kbd "s-=") #'todo-list-increase-font-size)
      (define-key map (kbd "s--") #'todo-list-decrease-font-size)
      (define-key map (kbd "s-0") #'todo-list-reset-font-size))
    map)
  "Keymap for `todo-list-mode'.")

;;; Internal variables

(defvar todo-list--files-open-p nil
  "Flag to track if the todo-list files are currently open.")

(defvar todo-list--saved-win-config nil
  "Variable to store the previous window configuration.")

;;; Internal functions

(defun todo-list--get-files ()
  "Return a list of file paths from `todo-list-files'.
Ensures paths are expanded to absolute paths."
  (let ((projects (plist-get todo-list-files :projects))
        (goals (plist-get todo-list-files :goals))
        files)
    (when projects
      (push (expand-file-name projects) files))
    (when goals
      (push (expand-file-name goals) files))
    (nreverse files)))

(defun todo-list--validate-files ()
  "Validate that both project and goal files are specified and exist.
Return non-nil if both files are valid, otherwise show error and return nil."
  (if (not (and (plist-member todo-list-files :projects)
                (plist-member todo-list-files :goals)))
    (user-error "`todo-list-files' requires both `:projects' and `:goals' entries")
    (let ((projects (expand-file-name (plist-get todo-list-files :projects)))
          (goals (expand-file-name (plist-get todo-list-files :goals))))
      (if (not (and (file-exists-p projects) (file-exists-p goals)))
        (user-error "Unable to read both file entries in `todo-list-files'")
        t))))

(defun todo-list--first-headline-recenter ()
  "Move to the first headline and recenter.
If no headline is found, stays at point-min."
  (goto-char (point-min))
  (when (re-search-forward org-heading-regexp nil t)
    (beginning-of-line)
    (recenter-top-bottom 0)))

(defun todo-list--kill-buffer-hook ()
  "Hook to run when a buffer with todo-list-mode is killed.
Restores window configuration when the last todo-list buffer is closed."
  (when todo-list--files-open-p
    (let ((files (todo-list--get-files)))
      (when (and files
                 (member (buffer-file-name) files)
                 (= 1 (length (seq-filter
                                (lambda (f)
                                  (get-buffer (file-name-nondirectory f)))
                                files))))
        ;; This is the last of our managed files being killed
        (setq todo-list--files-open-p nil)
        (when todo-list--saved-win-config
          (set-window-configuration todo-list--saved-win-config)
          (setq todo-list--saved-win-config nil))))))

;;; Navigation commands

(defun todo-list-next-section ()
  "Move to next top-level section in the current buffer."
  (interactive)
  (widen)
  (if (not (org-at-heading-p))
    ;; Not at heading - find first heading
    (progn
      (goto-char (point-min))
      (unless (re-search-forward "^\\* " nil t)
        (message "No headings in buffer")))
    ;; At heading - go to next top-level heading
    (let ((level (org-outline-level)))
      (if (= level 1)
        ;; At level 1 - try to move to next level 1
        (unless (org-forward-heading-same-level 1)
          (message "No next top-level heading"))
        ;; At deeper level - first go up to level 1
        (org-up-heading-all (1- level))
        ;; Then try to go forward
        (unless (org-forward-heading-same-level 1)
          (message "No next top-level heading"))))))

(defun todo-list-previous-section ()
  "Move to previous top-level section in the current buffer."
  (interactive)
  (widen)
  (if (not (org-at-heading-p))
    ;; Not at heading - find last heading
    (progn
      (goto-char (point-max))
      (unless (re-search-backward "^\\* " nil t)
        (message "No headings in buffer")))
    ;; At heading - go to previous top-level heading
    (let ((level (org-outline-level)))
      (if (= level 1)
        ;; At level 1 - try to move to previous level 1
        (unless (org-backward-heading-same-level 1)
          (message "No previous top-level heading"))
        ;; At deeper level - first go up to level 1
        (org-up-heading-all (1- level))
        ;; Then try to go backward
        (unless (org-backward-heading-same-level 1)
          (message "No previous top-level heading"))))))

;;; File management commands

;;;###autoload
(defun todo-list-toggle ()
  "Open and close `todo-list-mode' while preserving window configuration.
When closing, asks to save any modified buffers first."
  (interactive)
  (if todo-list--files-open-p
    (progn
      (setq todo-list--files-open-p nil)
      ;; Save modified buffers before closing
      (todo-list-save-buffers t)
      ;; Kill the buffers
      (dolist (file (todo-list--get-files))
        (when-let ((buffer (find-buffer-visiting file)))
          (kill-buffer buffer)))
      ;; Restore window configuration
      (when todo-list--saved-win-config
        (set-window-configuration todo-list--saved-win-config)
        (setq todo-list--saved-win-config nil)))
    (when (todo-list--validate-files)
      (setq todo-list--saved-win-config (current-window-configuration))
      (setq todo-list--files-open-p t)
      (todo-list-open-files))))

(defun todo-list-open-files ()
  "Open files from `todo-list-files' in side-by-side windows."
  (interactive)
  (if (not (todo-list--validate-files))
    (user-error "Invalid todo-list files configuration")
    (let ((projects (expand-file-name (plist-get todo-list-files :projects)))
          (goals (expand-file-name (plist-get todo-list-files :goals))))
      ;; Open the files in side-by-side windows
      (delete-other-windows)
      (find-file projects)
      (org-mode)
      (todo-list-mode 1)
      ;; Close all property drawers
      (org-cycle-hide-drawers 'all)
      ;; Start cursor at first entry
      (todo-list--first-headline-recenter)

      ;; Open second file on the right
      (split-window-right)
      (find-file-other-window goals)
      (org-mode)
      (todo-list-mode 1)
      ;; Close all property drawers
      (org-cycle-hide-drawers 'all)
      (todo-list--first-headline-recenter)

      ;; Return to projects window
      (select-window (get-buffer-window (find-file-noselect projects)))

      ;; Apply font sizing
      (todo-list-set-font-size))))

(defun todo-list-save-buffers (&optional no-confirm)
  "Save any modified buffers in files from `todo-list-files'.
When NO-CONFIRM is non-nil, save without asking for confirmation."
  (interactive)
  (dolist (file (todo-list--get-files))
    (let ((buffer (find-buffer-visiting file)))
      (when (and buffer (buffer-modified-p buffer))
        (with-current-buffer buffer
          (if no-confirm
            (save-buffer)
            (when (y-or-n-p (format "Save buffer %s? " (buffer-name)))
              (save-buffer))))))))

;;; Task management commands

;;;###autoload
(defun todo-list-entry-find ()
  "Find and jump to a heading within the project files.
Prompts for selection first, then opens the todo-list view if needed."
  (interactive)
  (unless todo-list-files
    (user-error "No todo-list files configured. Set `todo-list-files' first"))

  ;; Load files as needed
  (let ((files (todo-list--get-files))
        (entries '()))

    ;; Gather entries from all files - loading if needed
    (dolist (file files)
      (with-current-buffer (find-file-noselect file)
        (org-map-entries
          (lambda ()
            (let* ((pos (point))
                   (file-name (file-name-nondirectory (buffer-file-name)))
                   (raw-heading (org-get-heading t t t t))
                   ;; Remove text properties and trim whitespace
                   (heading (string-trim (substring-no-properties raw-heading)))
                   (display-name (format "%s | %s"
                                   heading (file-name-sans-extension file-name))))
              (push (list display-name file pos) entries)))
          nil nil)))

    ;; 1. Select entry first
    (let* ((choices (mapcar #'car entries))
           (selection (completing-read "Entry: " choices nil t)))

      (if-let ((entry (assoc selection entries)))
        (let ((target-file (nth 1 entry))
              (target-pos (nth 2 entry)))

          ;; 2. Open todo-list view if needed
          (unless todo-list--files-open-p
            (todo-list-toggle))

          ;; 3. Navigate to selected entry
          (let ((buffer (get-file-buffer target-file)))
            (when buffer
              (pop-to-buffer buffer)
              (goto-char target-pos)
              (org-fold-show-context 'agenda))))
        (user-error "Entry not found: %s" selection)))))

(defun todo-list-entry-add-properties ()
  "Add standard properties to the current heading.
Works anywhere within an entry. Adds TODO state if not present.
Requires being in an org buffer."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "This command only works in org-mode buffers"))
  (save-excursion
    (org-back-to-heading t)
    ;; Add TODO keyword if not present
    (let ((heading (org-get-heading t t t t)))
      (when (not (string-match-p "^TODO " heading))
        (org-todo "TODO")))

    ;; Add ID and other properties
    (org-id-get-create)
    (org-entry-put nil "CATEGORY" "task")

    ;; Only add CREATED timestamp if not already present
    (unless (org-entry-get nil "CREATED")
      (org-entry-put nil "CREATED" (format-time-string "[%Y-%m-%d %a %H:%M]")))

    (message "Added properties to current entry")))

;;; Font size commands

(defun todo-list-set-font-size ()
  "Set the font size according to `todo-list-font-size'.
Applies to all open todo-list buffers."
  (dolist (file (todo-list--get-files))
    (let ((buffer (find-buffer-visiting file)))
      (when buffer
        (with-current-buffer buffer
          (text-scale-set todo-list-font-size))))))

(defun todo-list-increase-font-size ()
  "Increase the font size in all todo-list buffers."
  (interactive)
  (setq todo-list-font-size (1+ todo-list-font-size))
  (todo-list-set-font-size))

(defun todo-list-decrease-font-size ()
  "Decrease the font size in all todo-list buffers."
  (interactive)
  (setq todo-list-font-size (1- todo-list-font-size))
  (todo-list-set-font-size))

(defun todo-list-reset-font-size ()
  "Reset the font size to the default value in all todo-list buffers."
  (interactive)
  (custom-reevaluate-setting 'todo-list-font-size)
  (todo-list-set-font-size))

;;; Mode definition

;;;###autoload
(define-minor-mode todo-list-mode
  "Toggle Todo-List mode.
A minor mode for managing projects and goals with org-mode."
  :lighter " todo"
  :keymap todo-list-mode-map
  ;; Only add/remove hook when toggling, not during mode initialization
  (if (not (eq todo-list-mode (bound-and-true-p todo-list-mode)))
    (if todo-list-mode
      (add-hook 'kill-buffer-hook #'todo-list--kill-buffer-hook nil t)
      (remove-hook 'kill-buffer-hook #'todo-list--kill-buffer-hook t))))

(provide 'todo-list-mode)
;;; todo-list-mode.el ends here
