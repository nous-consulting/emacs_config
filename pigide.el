(load-library "vtags")
(require 'py-imports)
(require 'flymake)

(defvar *pigide-active-project*)

(defun mako-i18nize ()
  (interactive)
  (kill-region (mark) (point))
  (insert "${_('')}")
  (backward-char-nomark 3)
  (yank))

(defun pigide-ensure-project ()
  (if *pigide-active-project*
      *pigide-active-project*
      (pigide-select-project)))

(defun pigide-select-project ()
  (interactive)
  (setq *pigide-active-project*
        (expand-file-name
         (read-directory-name "Select active project:"
                              default-directory))))

(defun pigide-buildout ()
  (interactive)
  (compile (format "cd %s && bin/buildout" (pigide-ensure-project))))

(defun pigide-tags ()
  (interactive)
  (compile (format "cd %s && bin/tags" (pigide-ensure-project))))

(defun pigide-ctags ()
  (interactive)
  (compile (format "cd %s && bin/ctags" (pigide-ensure-project))))

(defvar pigide-test-command "test")
(defvar pigide-test-history nil)
(defun pigide-test ()
  (interactive)
  (let ((command
         (read-from-minibuffer "Test command: "
                               pigide-test-command nil nil
                               (if (equal (car pigide-test-history) pigide-test-command)
                                   '(pigide-test-history . 1)
                                   'pigide-test-history))))
    (compile (format "cd %s && bin/%s" (pigide-ensure-project) command) t)
    (setq pigide-test-command command)))

(defvar pigide-compile-command "gid")
(defvar pigide-compile-history nil)
(defun pigide-compile ()
  (interactive)
  (let ((command
         (read-from-minibuffer "Compile command: "
                               pigide-compile-command nil nil
                               (if (equal (car pigide-compile-history) pigide-compile-command)
                                   '(pigide-compile-history . 1)
                                   'pigide-compile-history))))
    (let ((default-directory (pigide-ensure-project))
          (compilation-environment '("PAGER=")))
      (compile (format "%s" command) t))
    (setq pigide-compile-command command)))

;;;
;;; Guessing the right "thing" from the cursor position, use thing-at-point.
;;;  - could be refined, when the need arises
;;;
(defvar pigide-python-identifier-re "\\(?:[a-zA-Z_][a-zA-Z0-9_]*\\)"
  "Match a python identifier.")

(defvar pigide-python-dotexpr-re
  (format "\\b%s\\(?:\\.%s\\)*"
          pigide-python-identifier-re
          pigide-python-identifier-re)
  "Match a python expressions consisting of x.y.z")

(defun pigide-python-dotexpr-begin ()
  (interactive)
  (re-search-backward "[^a-zA-Z_0-9\\.]")
  (forward-char))

(defun pigide-python-dotexpr-end ()
  (interactive)
  (re-search-forward "[a-zA-Z_0-9\\.]*"))

(put 'python-dotexpr 'beginning-op 'pigide-python-dotexpr-begin)
(put 'python-dotexpr 'end-op 'pigide-python-dotexpr-end)

(defun cleanup-sym (sym)
  (with-temp-buffer
    (insert sym)
    (hacky-regexp-replace ".*\\.\\(.*\\)" "\\1")
    (buffer-string)))

(defun get-class-name (line)
  (with-temp-buffer
    (insert line)
    (hacky-regexp-replace ".*\tm\tclass:\\(.*\\)\n" "\\1")
    (buffer-string)))

(defun cleanup-class-signature (signature class)
  (with-temp-buffer
    (insert (cleanup-signature signature))
    (hacky-replace "__init__" "")
    (beginning-of-line)
    (insert class)
    (buffer-string)))

;; Improve by ignoring interfaces and class definitions and tracking
;; multiple possible constructors
(defvar class-sig-map (make-hash-table :test 'equal))

(defun get-class-sig-map (tfi)
  (let ((file (tagfileinfo-file tfi)))
    (if (gethash file class-sig-map)
        (gethash file class-sig-map)
        (puthash file (generate-class-sig-map tfi) class-sig-map))))

(defun class-signature (sym)
  (gethash sym (get-class-sig-map (find-tags-table-heuristically))))

(defun generate-class-sig-map (tfi)
  (let ((signatures (make-hash-table :test 'equal))
        (pattern "__init__"))
    (save-excursion
      (let ((dir (file-name-directory (tagfileinfo-file tfi))))
        (vtags-look pattern tfi
                    (lambda (line count)
                      (when (equal (tagEntryInfo-name (vtags-parse-line line)) pattern)
                        (puthash (get-class-name line)
                                 (cleanup-class-signature (tagEntryInfo-pattern (vtags-parse-line line))
                                                          (get-class-name line))
                                 signatures))))))
    signatures))

(defun is-python-class (signature)
  (with-temp-buffer
    (insert signature)
    (beginning-of-line)
    (let ((case-fold-search nil))
      (looking-at "[[:upper:]]"))))

(defun pigide-signature (sym)
  (let ((signature (car (find-all-signatures-of-tag-vtags (cleanup-sym sym)))))
    (when signature
      (if (is-python-class signature)
          (class-signature sym)
          signature))))

(defun pigide-signature-show ()
  (interactive)
  (require 'thingatpt)
  (let ((sym (thing-at-point 'python-dotexpr)))
    (when (and sym (not (string= sym "")))
      (message sym)
      (let ((signature (pigide-signature sym)))
        (set 'pigide-current-signature signature)
        (when signature
          (pigide-show signature))))))

(defun pigide-show (string)
  (display-message-or-buffer string "*PythonHelp*"))

;; keys
(defun pigide-electric-lparen ()
  "Electricly insert '(', and try to get a signature for the stuff to the left."
  (interactive)
  (pigide-signature-show)
  (self-insert-command 1))

(defun pigide-electric-comma ()
  "Electricly insert ',', and redisplay latest signature."
  (interactive)
  (self-insert-command 1)
  (if pigide-current-signature
      (pigide-show (format "%s" pigide-current-signature))))

;;(defun pigide-setup-keys ()
;;  "Setup some standard keys for pigide."
;;  (mapcar
;;   (lambda (args)
;;     (apply 'define-key (cons python-mode-map args)))
;;   '(
;;    ("(" pigide-electric-lparen)
;;    ("," pigide-electric-comma)
;;    )))

;;(pigide-setup-keys)

(defun cleanup-signature (signature)
  (with-temp-buffer
    (insert signature)
    (hacky-regexp-replace "^\\^ *def " "")
    (beginning-of-line)
    (hacky-regexp-replace "^\\^ *class " "")
    (beginning-of-line)
    (hacky-regexp-replace "\(self, " "(")
    (beginning-of-line)
    (hacky-regexp-replace ":\\$" "")
    (beginning-of-line)
    (buffer-string)))

(defvar pigide-predefined-signatures
  '(("getUtility" "getUtility(provided, name=u'')")
    ("__init__" "")
    ("__call__" "")))

(defun lookup-predefined-signature (pattern)
  (loop for import in pigide-predefined-signatures
       when (equal (car import)
                   pattern)
       return (cdr import)))

(defun find-all-signatures-of-tag-vtags (pattern)
  (or (lookup-predefined-signature pattern)
      (let ((tfi (find-tags-table-heuristically))
            (files))
        (save-excursion
          (let ((dir (file-name-directory (tagfileinfo-file tfi))))
            (vtags-look pattern tfi
                        (lambda (line count)
                          (when (equal (tagEntryInfo-name (vtags-parse-line line))
                                       pattern)
                            (push
                             (cleanup-signature (tagEntryInfo-pattern (vtags-parse-line line)))
                             files))))))
        files)))

(defun pigide-forward-sexp-func (&optional count)
  ;; write this thing for python
  ;; define it as buffer local for python-mode
  )

;;; (add-hook 'lisp-mode-hook
;;;   (lambda ()
;;;     (set (make-local-variable 'lisp-indent-function)
;;;          'common-lisp-indent-function)
;;;     (modify-syntax-entry 58 "w")
;;;     (local-set-key (kbd "<tab>")
;;;                    'lisp-indent-or-complete)))


(defun flymake-pyflakes-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "pyflakes" (list local-file))))

(defun show-fly-err-at-point ()
  "If the cursor is sitting on a flymake error, display the message in the minibuffer"
  (interactive)
  (let ((line-no (line-number-at-pos)))
    (dolist (elem flymake-err-info)
      (if (eq (car elem) line-no)
          (let ((err (car (second elem))))
            (message "%s" (flymake-ler-text err)))))))

(add-hook 'post-command-hook 'show-fly-err-at-point)

(defun match-adapter (adapter)
  (let ((tfi (find-tags-table-heuristically))
        (files)
        (pattern (cleanup-sym adapter)))
    (save-excursion
      (let ((dir (file-name-directory (tagfileinfo-file tfi))))
        (vtags-look pattern tfi
                    (lambda (line count)
                      (when (and (equal (tagEntryInfo-name (vtags-parse-line line))
                                        pattern)
                                 (equal (process-import-in-place (tagEntryInfo-file (vtags-parse-line line)) pattern)
                                        adapter))
                        (push line
                              files))))))
    (car files)))

;;(match-adapter "schooltool.app.app.getSchoolToolApplication")

(defun get-adapters (interface)
  (let* ((url-template "http://localhost:7080/++apidoc++/Interface/%s/adapters.html")
         (buffer (url-retrieve-synchronously (format url-template interface)))
         (adapters))
    (when buffer
      (with-current-buffer buffer
        (beginning-of-buffer)
        (search-forward "\r\n\r\n")
        (let ((done nil))
          (while (and (not done)
                      (not (= (point) (point-max))))
            (let* ((beg (progn (beginning-of-line) (point)))
                   (end (progn (end-of-line) (point))))
              (setq line (buffer-substring-no-properties beg end))
              (when (> (length line) 1)
                (let ((adapter (match-adapter line)))
                  (when adapter
                    (push adapter adapters))))
              (forward-line 1)))))
      (list-adapters adapters interface))))

(defun list-adapters (adapters interface)
  (let ((out-buf (get-buffer-create "*Adapter List*"))
        (gotany nil)
        (dir (file-name-directory (tagfileinfo-file (find-tags-table-heuristically)))))
    (set-buffer out-buf)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert-string "Adapters implementing interface `")
    (insert-string interface)
    (insert-string "':\n\n")

    (dolist (tag-line adapters)
      (setq entry (vtags-parse-line tag-line))
      (setq tag (tagEntryInfo-name entry))
      (vtags-set-action-properties entry tag dir)
      (insert-string (concat tag "\n" )))

    (switch-to-buffer out-buf)
    (goto-char (point-min))
    (forward-line 1)
    (vtags-mode)
    (setq buffer-read-only t)))


;;(get-adapters "schooltool.person.interfaces.IPerson")

(defun lookup-adapter (interface)
  (get-adapters (find-all-imports-to-expand interface)))

;;(lookup-adapter "IPathAdapter")

(defun pigide-setup ()
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))

(defun cleanup-test (test)
  (with-temp-buffer
    (insert test)
    (hacky-regexp-replace ".*/src/" "/src/")
    (hacky-replace "Tests with failures:" "")
    (hacky-replace "Tests with errors:" "")
    (hacky-regexp-replace "^ +" "")
    (hacky-regexp-replace "\n" "")
    (buffer-string)))

(defun quote-parens (test)
  (with-temp-buffer
    (insert test)
    (hacky-replace "(" "\\(")
    (hacky-replace ")" "\\)")
    (buffer-string)))

(defun cleanup-command (command)
  (with-temp-buffer
    (insert command)
    (hacky-regexp-replace " .*" "")
    (buffer-string)))

(defun cleanup-test-list (tests)
  (loop for test in
       (loop for test in tests
          when test
          collect (cleanup-test test))
       when (not (string-equal test ""))
       collect (format "^%s$" (quote-parens (regexp-quote test)))))

(defun pigide-test-region ()
  (interactive)
  (let ((command
         (save-excursion
           (let ((start (min (mark) (point)))
                 (end (max (mark) (point)))
                 (tests))
             (goto-char start)
             (while (< (point) end)
               (beginning-of-line)
               (push (thing-at-point 'line) tests)
               (next-line))
             (with-temp-buffer
               (insert (cleanup-command pigide-test-command))
               (dolist (test (cleanup-test-list tests))
                 (insert " -t ")
                 (insert test))
               (message (buffer-string)))))))
    (compile (format "cd %s && bin/%s" (pigide-ensure-project) command) t)
    (setq pigide-test-command command)))

(provide 'pigide)
