;;; sym-comp.el --- mode-dependent symbol completion

;; Copyright (C) 2004  Dave Love

;; Author: Dave Love <fx@gnu.org>
;; Keywords: extensions

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This defines `symbol-complete', which is a generalization of the
;; old `lisp-complete-symbol'.  It provides the following hooks to
;; allow major modes to set up completion appropriate for the mode:
;; `symbol-completion-symbol-function',
;; `symbol-completion-completions-function',
;; `symbol-completion-predicate-function',
;; `symbol-completion-transform-function'.  Typically it is only
;; necessary for a mode to set
;; `symbol-completion-completions-function' locally and to bind
;; `symbol-complete' appropriately.

;; It's unfortunate that there doesn't seem to be a good way of
;; combining this with `complete-symbol'.

;; There is also `symbol-completion-try-complete', for use with
;; Hippie-exp.

;; As an example, `lisp-complete-symbol' is defined below using this
;; infrastructure to replace the old one from lisp.el with the same
;; functionality.

;;; Code:

;;;; Mode-dependent symbol completion.

(defun symbol-completion-symbol ()
  "Default `symbol-completion-symbol-function'.
Uses `current-word' with the buffer narrowed to the part before
point."
  (save-restriction
    ;; Narrow in case point is in the middle of a symbol -- we want
    ;; just the preceeding part.
    (narrow-to-region (point-min) (point))
    (current-word)))

(defvar symbol-completion-symbol-function 'symbol-completion-symbol
  "Function to return a partial symbol before point for completion.
The value it returns should be a string (or nil).
Major modes may set this locally if the default isn't appropriate.")

(defvar symbol-completion-completions-function nil
  "Function to return possible symbol completions.
It takes an argument which is the string to be completed and
returns a value suitable for the second argument of
`try-completion'.  This value need not use the argument, i.e. it
may be all possible completions, such as `obarray' in the case of
Emacs Lisp.

Major modes may set this locally to allow them to support
`symbol-complete'.  See also `symbol-completion-symbol-function',
`symbol-completion-predicate-function' and
`symbol-completion-transform-function'.")

(defvar symbol-completion-predicate-function nil
  "If non-nil, function to return a predicate for selecting symbol completions.
The function gets two args, the positions of the beginning and
end of the symbol to be completed.

Major modes may set this locally if the default isn't
appropriate.  This is a function returning a predicate so that
the predicate can be context-dependent, e.g. to select only
function names if point is at a function call position.  The
function's args may be useful for determining the context.")

(defvar symbol-completion-transform-function nil
  "If non-nil, function to transform symbols in the symbol-completion buffer.
E.g., for Lisp, it may annotate the symbol as being a function,
not a variable.

The function takes the symbol name as argument.  If it needs to
annotate this, it should return a value suitable as an element of
the list passed to `display-completion-list'.

The predicate being used for selecting completions (from
`symbol-completion-predicate-function') is available
dynamically-bound as `symbol-completion-predicate' in case the
transform needs it.")

(defvar displayed-completions)

;;;###autoload
(defun symbol-complete (&optional predicate)
  "Perform completion of the symbol preceding point.
This is done in a way appropriate to the current major mode,
perhaps by interrogating an inferior interpreter.  Compare
`complete-symbol'.
If no characters can be completed, display a list of possible completions.
Repeating the command at that point scrolls the list.

When called from a program, optional arg PREDICATE is a predicate
determining which symbols are considered.

This function requires `symbol-completion-completions-function'
to be set buffer-locally.  Variables `symbol-completion-symbol-function',
`symbol-completion-predicate-function' and
`symbol-completion-transform-function' are also consulted."
  (interactive)
  ;; Fixme: Punt to `complete-symbol' in this case?
  (unless (functionp symbol-completion-completions-function)
    (error "symbol-completion-completions-function not defined"))
  (let ((window (get-buffer-window "*Completions*")))
    (let* ((pattern (or (funcall symbol-completion-symbol-function)
			(error "No preceding symbol to complete")))
	   (predicate (or predicate
			  (if symbol-completion-predicate-function
			      (funcall symbol-completion-predicate-function
				       (- (point) (length pattern))
				       (point)))))
	   (completions (funcall symbol-completion-completions-function
				 pattern))
	   (completion (try-completion pattern completions predicate)))
      ;; If this command was repeated, and there's a fresh completion
      ;; window with a live buffer and a displayed completion list
      ;; matching the current completions, then scroll the window.
      (unless (and (eq last-command this-command)
		   window (window-live-p window) (window-buffer window)
		   (buffer-name (window-buffer window))
		   (with-current-buffer (window-buffer window)
		     (if (equal displayed-completions
				(all-completions pattern completions predicate))
			 (progn
			   (if (pos-visible-in-window-p (point-max) window)
			       (set-window-start window (point-min))
			     (save-selected-window
			       (select-window window)
			       (scroll-up)))
			   t))))
	;; Otherwise, do completion.
	(cond ((eq completion t))
	      ((null completion)
	       (message "Can't find completion for \"%s\"" pattern)
	       (ding))
	      ((not (string= pattern completion))
	       (delete-region (- (point) (length pattern)) (point))
	       (insert completion))
	      (t
	       (message "Making completion list...")
	       (let* ((list (all-completions pattern completions predicate))
		      ;; In case the transform needs to access it.
		      (symbol-completion-predicate predicate)
		      ;; Copy since list is side-effected by sorting.
		      (copy (copy-sequence list)))
		 (setq list (sort list 'string<))
		 (if (functionp symbol-completion-transform-function)
		     (setq list
			   (mapcar (funcall
				    symbol-completion-transform-function)
				   list)))
		 (with-output-to-temp-buffer "*Completions*"
		   (display-completion-list list))
		 ;; Record the list for determining whether to scroll
		 ;; (above).
		 (with-current-buffer "*Completions*"
		   (set (make-local-variable 'displayed-completions) copy)))
	       (message "Making completion list...%s" "done")))))))

(eval-when-compile (require 'hippie-exp))

;;;###autoload
(defun symbol-completion-try-complete (old)
  "Completion function for use with `hippie-expand'.
Uses `symbol-completion-symbol-function' and
`symbol-completion-completions-function'.  It is intended to be
used something like this in a major mode which provides symbol
completion:

  (if (featurep 'hippie-exp)
      (set (make-local-variable 'hippie-expand-try-functions-list)
	   (cons 'symbol-completion-try-complete
                 hippie-expand-try-functions-list)))"
  (when (and symbol-completion-symbol-function
	     symbol-completion-completions-function)
    (unless old
      (let ((symbol (funcall symbol-completion-symbol-function)))
	(he-init-string (- (point) (length symbol)) (point))
	(if (not (he-string-member he-search-string he-tried-table))
	    (push he-search-string he-tried-table))
	(setq he-expand-list
	      (and symbol
		   (funcall symbol-completion-completions-function symbol)))))
    (while (and he-expand-list
		(he-string-member (car he-expand-list) he-tried-table))
      (pop he-expand-list))
    (if he-expand-list
	(progn
	  (he-substitute-string (pop he-expand-list))
	  t)
      (if old (he-reset-string))
      nil)))

;;; Emacs Lisp symbol completion.

(defun lisp-completion-symbol ()
  "`symbol-completion-symbol-function' for Lisp."
  (let ((end (point))
	(beg (with-syntax-table emacs-lisp-mode-syntax-table
	       (save-excursion
		 (backward-sexp 1)
		 (while (= (char-syntax (following-char)) ?\')
		   (forward-char 1))
		 (point)))))
    (buffer-substring-no-properties beg end)))

(defun lisp-completion-predicate (beg end)
  "`symbol-completion-predicate-function' for Lisp."
  (save-excursion
    (goto-char beg)
    (if (not (eq (char-before) ?\())
	(lambda (sym)			;why not just nil ?   -sm
					;To avoid interned symbols with
					;no slots.  -- fx
	  (or (boundp sym) (fboundp sym)
	      (symbol-plist sym)))
      ;; Looks like a funcall position.  Let's double check.
      (if (condition-case nil
	      (progn (up-list -2) (forward-char 1)
		     (eq (char-after) ?\())
	    (error nil))
	  ;; If the first element of the parent list is an open
	  ;; parenthesis we are probably not in a funcall position.
	  ;; Maybe a `let' varlist or something.
	  nil
	;; Else, we assume that a function name is expected.
	'fboundp))))

(defvar symbol-completion-predicate)

(defun lisp-symbol-completion-transform ()
  "`symbol-completion-transform-function' for Lisp."
  (lambda (elt)
    (if (and (not (eq 'fboundp symbol-completion-predicate))
	     (fboundp (intern elt)))
	(list elt " <f>")
      elt)))

;; We don't just have the hooks as buffer-local variables in
;; emacs-lisp-mode since the original version of this function worked
;; in all buffers.  Thus just bind them locally.

;;;###autoload
(defun lisp-complete-symbol (&optional predicate)
  "Perform completion on Lisp symbol preceding point.
Compare that symbol against the known Lisp symbols.
If no characters can be completed, display a list of possible completions.
Repeating the command at that point scrolls the list.

When called from a program, optional arg PREDICATE is a predicate
determining which symbols are considered, e.g. `commandp'.
If PREDICATE is nil, the context determines which symbols are
considered.  If the symbol starts just after an open-parenthesis, only
symbols with function definitions are considered.  Otherwise, all
symbols with function definitions, values or properties are
considered."
  (interactive)
  (let ((symbol-completion-symbol-function #'lisp-completion-symbol)
	(symbol-completion-completions-function (lambda (sym) obarray))
	(symbol-completion-predicate-function #'lisp-completion-predicate)
	(symbol-completion-transform-function
	 #'lisp-symbol-completion-transform))
    (symbol-complete predicate)))

(provide 'sym-comp)
;;; sym-comp.el ends here
