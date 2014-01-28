;; todo-list-mode.el
;; Emacs major-mode for highlighting a plain-text, numbered todo list.
;;
;; Copyright (C) 2009 Billy Lamberta
;;
;; Author: Billy Lamberta <b@lamberta.org>
;; Created: Jan 2009
;; Updated: Jan 2014
;; Keywords: todo
;; $Revision: 0.3 $
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
;; Major-mode for highlighting a plain-text, numbered todo list.
;;
;; A simple mode that uses a few regexp's to determine the color of
;; a line based on the assigned priority number. Files are auto-sorted
;; and saved after a new entry.
;;
;; In your .emacs:
;;     (add-to-list 'load-path "~/my-lisp-path/")
;;     (autoload 'todo-list-mode "todo-list-mode")
;;
;; To use: M-x todo-list-mode
;;
;; To access it at the push of a button:
;;     (defun open-todo-list ()
;;       "Open up a todo list in todo-list-mode."
;;       (interactive)
;;       (find-file "~/path-to/todo.txt")
;;       (todo-list-mode))
;;     (global-set-key [f12] 'open-todo-list) ;shortcut
;;
;; The format for a given line is:
;; [number] [whitespace] [text]
;;
;; The priority number can include an optional lowercase letter after it.
;; A line starting with 'C' is marked as complete.
;; Hash-style comments are highlighted to the end of the line.
;;
;; Example:
;; 0  Goal: Build a Time Machine
;; 1a Find a DeLorean
;; 1b Acquire plutonium #watch out for Libyan terrorists
;; 2  Read docs on flux capacitors #1.21 gigawatts!
;; 3  Try not to cause too many problems while traveling
;; 4  Get bulletproof vest for Doc
;; C  Learn Johnny B. Goode on guitar
;;
;; Regexps and faces are defined in this file below. Feel free to add
;; more or change the colors to suit your own style.
;;
;; Navigation:
;; TAB    Jump between columns
;; RETURN When cursor at end of line, automatically sort buffer

(setq todo-list-highlight-regexps '(
   ("^0[a-z]?[\t ].*$" 0 todo-list-zero-face t)  ;0 ...
   ("^1[a-z]?[\t ].*$" 0 todo-list-one-face t)   ;1 ...
   ("^2[a-z]?[\t ].*$" 0 todo-list-two-face t)   ;2 ...
   ("^3[a-z]?[\t ].*$" 0 todo-list-three-face t) ;3 ...
   ("^4[a-z]?[\t ].*$" 0 todo-list-four-face t)  ;4 ...
   ("^[Cc][\t ].*$" 0 todo-list-complete-face t) ;task completed
   ("#.*$" 0 font-lock-comment-face t)))         ;comments

(define-derived-mode todo-list-mode fundamental-mode
  "todo-list-mode"
  "Major mode for syntax color highlighting of a numbered todo list."
  (setq font-lock-defaults '(todo-list-highlight-regexps)))

(setq require-final-newline t)

;;
;; font faces for todo entries
;;

(defface todo-list-zero-face'(
  (((class color) (background dark)) (:foreground "white" :slant italic))
  (((class color) (background light)) (:foreground "black" :slant italic))
  (t (:bold t :italic t)))
  "Todo-List mode face used for level 0 task."
  :group 'todo-list-mode-highlighting-faces)
(defvar todo-list-zero-face 'todo-list-zero-face
  "Todo-List mode face used for level 0 task.")

(defface todo-list-one-face'(
  (((class color) (background dark))  (:foreground "IndianRed1"))
  (((class color) (background light)) (:foreground "Red3"))
  (t (:bold t :italic t)))
  "Todo-List mode face used for level 1 task."
  :group 'todo-list-mode-highlighting-faces)
(defvar todo-list-one-face 'todo-list-one-face
  "Todo-List mode face used for level 1 task.")

(defface todo-list-two-face'(
  (((class color) (background dark))  (:foreground "CadetBlue2"))
  (((class color) (background light)) (:foreground "Blue3"))
  (t (:bold t :italic t)))
  "Todo-List mode face used for level 2 task."
  :group 'todo-list-mode-highlighting-faces)
(defvar todo-list-two-face 'todo-list-two-face
  "Todo-List mode face used for level 2 task.")

(defface todo-list-three-face'(
  (((class color) (background dark))  (:foreground "MistyRose2"))
  (((class color) (background light)) (:foreground "DarkOrange3"))
  (t (:bold t :italic t)))
  "Todo-List mode face used for level 3 task."
  :group 'todo-list-mode-highlighting-faces)
(defvar todo-list-three-face 'todo-list-three-face
  "Todo-List mode face used for level 3 task.")

(defface todo-list-four-face'(
  (((class color) (background dark))  (:foreground "LightSteelBlue1"))
  (((class color) (background light)) (:foreground "DodgerBlue2"))
  (t (:bold t :italic t)))
  "Todo-List mode face used for level 4 task."
  :group 'todo-list-mode-highlighting-faces)
(defvar todo-list-four-face 'todo-list-four-face
  "Todo-List mode face used for level 4 task.")

(defface todo-list-complete-face'(
  (((class color) (background dark))  (:foreground "#999"))
  (((class color) (background light)) (:foreground "gray75"))
  (t (:bold t :italic t)))
  "Todo-List mode face used for completed task."
  :group 'todo-list-mode-highlighting-faces)
(defvar todo-list-complete-face 'todo-list-complete-face
  "Todo-List mode face used for completed task.")

;;
;; convenience functions
;;

(defun todo-list-sort-buffer ()
  "Run sort-lines on entire buffer and remove empty lines."
  (interactive)
  (mark-whole-buffer)
  (flush-lines "^$")
  (sort-lines nil (region-beginning) (region-end))
  (delete-trailing-whitespace))

(defun todo-list-cursor-jump ()
  "Quick cursor jump between formatting columns."
  (interactive)
  (if (>= (current-column) 3)
    (move-beginning-of-line 1)
    (progn
      (move-end-of-line 1)
      (if (< (current-column) 3)
        (indent-to 3)
        (progn
          (move-beginning-of-line 1)
          (forward-char 3))))))

(define-key todo-list-mode-map (kbd "<return>")
  '(lambda ()
     "Sort todo-list buffer at end of line, otherwise insert newline."
     (interactive)
     (let ((next-char (char-after)))
       (if (or (null next-char) (char-equal next-char ?\n))
         (todo-list-sort-buffer)
         (newline)))))

(define-key todo-list-mode-map (kbd "C-<return>") 'newline)
(define-key todo-list-mode-map (kbd "<tab>") 'todo-list-cursor-jump)
 
(provide 'todo-list-mode)
