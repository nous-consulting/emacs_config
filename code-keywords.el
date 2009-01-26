;;; code-keywords.el --- highlight code keywords

;; Copyright (C) 1997-2000 Free Software Foundation, Inc.

;; Author: Kevin A. Burton (burton@openprivacy.org)
;; Maintainer: Kevin A. Burton (burton@openprivacy.org)
;; Location: http://relativity.yi.org
;; Keywords: code highlight keywords
;; Version: 1.0.0

;; This file is [not yet] part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free Software Foundation, Inc., 59 Temple
;; Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; Software Engineers/Hackers often use words like "FIXME" or "TODO" or "HACK"
;; when developing software.  code-keywords is an attempt to highlight these
;; words so that they are more obvious.

;;; Install:

;; (require 'code-keywords) in your .emacs

;;; History:

;;; Code:

(defface code-keywords-serious-face '((t (:foreground "red"))) "Serious keywords.")

(defface code-keywords-warning-face '((t (:foreground "BlueViolet"))) "Warning keywords.")

(defface code-keywords-accent-face '((t (:italic t))) "Accent keywords.")

(defface code-keywords-line-face '((t (:background "sienna4")))
  "Face to use when matching the entire line..")

(defcustom code-keywords-serious-keywords (list "FIXME" "HACK")
  "*Specify the keywords to highlight with the `code-keywords-serious-face'."
  :type '(repeat string)
  :group 'code-keywords)

(defcustom code-keywords-warning-keywords (list "WARNING" "NOTE" "TODO")
  "*Specify the keywords to highlight with the `code-keywords-warning-face'."
  :type '(repeat string)
  :group 'code-keywords)

(defcustom code-keywords-accent-keywords (list "DONE" "PENDING")
  "*Specify the keywords to highlight with the `code-keywords-accent-face'."
  :type '(repeat string)
  :group 'code-keywords)

(defcustom code-keyword-hightlight-line nil
  "*When non-nil, highlight the entire line with `code-keywords-line-face'."
  :type 'boolean
  :group 'code-keywords)

(defvar code-keywords-no-boundary-regexp "[^a-zA-Z0-9/,-]"
  "Contains a regexp for words we should not match next to.")

(defun code-keywords-font-lock-add-keywords(strings face)
  "Add the given keyword with the given `strings' and aply the given face."

  ;;we need to build a regexp that matches each one of the words in the list but
  ;;not when next to other words.  AKA DONE but not asdfDONE.
  (font-lock-add-keywords nil
                          (list
                           (list (concat code-keywords-no-boundary-regexp
                                         "\\(\\("
                                         (regexp-opt strings)
                                         "\\)\\)"
                                         code-keywords-no-boundary-regexp) 1 face 'append))))

(defun code-keywords-font-lock-entire-line(strings)
  "Given a list of strings, highlight all lines with them using the
  `code-keywords-line-face' face."

  (when code-keyword-hightlight-line
  
    (dolist(current strings)
      
      (font-lock-add-keywords nil
                              (list
                               (list (concat "^.*"
                                             code-keywords-no-boundary-regexp
                                             current
                                             code-keywords-no-boundary-regexp
                                             ".*\n") 0 ''code-keywords-line-face 'append))))))

(defun code-keywords--font-lock()
  "Adds a font-lock for buffers that user opens."
  (interactive)

  ;;only highlight buffers that aren't special
  (if (and (not (string-match "\\*"
                              (buffer-name (current-buffer))))
           (not (equal major-mode 'records-mode))
           (not (equal major-mode 'dired-mode)))
      (progn

        (code-keywords-font-lock-add-keywords code-keywords-serious-keywords ''code-keywords-serious-face)
        
        (code-keywords-font-lock-add-keywords code-keywords-warning-keywords ''code-keywords-warning-face)

        (code-keywords-font-lock-add-keywords code-keywords-accent-keywords ''code-keywords-accent-face)

        ;;ok... highlight all lines
        (code-keywords-font-lock-entire-line (append code-keywords-serious-keywords
                                                     code-keywords-warning-keywords
                                                     code-keywords-accent-keywords)))))

;;add a hook to font-lock-mode so that we can add our keywords.
(add-hook 'font-lock-mode-hook 'code-keywords--font-lock)

(provide 'code-keywords)

;;; code-keywords.el ends here
