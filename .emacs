(add-to-list 'load-path "~/.site-lisp/")
(add-to-list 'load-path "~/.site-lisp/extra/")
(add-to-list 'load-path "~/.site-lisp/extra/ledger")

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(auto-compression-mode t nil (jka-compr))
 '(backup-by-copying nil)
 '(backup-by-copying-when-mismatch nil)
 '(c-basic-offset 4)
 '(c-default-style (quote ((c-mode . "bsd") (c++-mode . "bsd") (java-mode . "java") (other . "bsd"))))
 '(c-tab-always-indent t)
 '(canlock-password "cd97cbb4e23fbb87330e0ae4102c7aae4d4e5fbd")
 '(case-fold-search t)
 '(compilation-mode-hook nil)
 '(dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\([^.].*\\)")
 '(dired-omit-files-p t t)
 '(dired-omit-mode t t)
 '(dired-omit-size-limit nil)
 '(dired-recursive-copies (quote top))
 '(dired-recursive-deletes (quote top))
 '(fill-column 70)
 '(global-font-lock-mode t nil (font-lock))
 '(indent-tabs-mode nil)
 '(ispell-local-dictionary "british")
 '(ispell-program-name "ispell")
 '(jde-jdk-registry (quote (("1.4.2" . "/usr/lib/j2se/1.4/"))))
 '(ls-lisp-dirs-first t)
 '(ls-lisp-emulation nil)
 '(ls-lisp-hide-\.lnk t)
 '(ls-lisp-ignore-case t)
 '(make-backup-files t)
 '(pascal-indent-level 2)
 '(prolog-program-name "pl")
 '(require-final-newline (quote ask))
 '(safe-local-variable-values (quote ((test-case-name . pyflakes) (fill-column) (indent-tabs) (encoding . utf-8) (py-indent-offset . 2) (py-indent-offset . 3) (Package SERIES :use "COMMON-LISP" :colon-mode :external) (Encoding . utf-8) (Readtable . GLISP) (unibyte . t) (lisp-version . "7.0 [Windows] (Apr 6, 2005 17:03)") (cg . "1.54.2.17") (Log . clx\.log) (Lowercase . Yes) (kept-old-versions . 0))))
 '(save-place t nil (saveplace))
 '(scroll-bar-mode (quote right))
 '(sgml-basic-offset 2)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(speedbar-use-images nil)
 '(speedbar-use-tool-tips-flag nil)
 '(tags-case-fold-search nil)
 '(tex-dvi-view-command "gv")
 '(tool-bar-mode nil nil (tool-bar))
 '(tramp-default-method "scp")
 '(transient-mark-mode t)
 '(truncate-lines t))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "black" :foreground "grey85" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "misc" :family "fixed"))))
 '(my-long-line-face ((((class color)) (:background "gray20"))) t)
 '(my-tab-face ((((class color)) (:background "grey20"))) t)
 '(nxml-attribute-local-name-face ((t (:inherit font-lock-variable-name-face))))
 '(nxml-attribute-value-delimiter-face ((t (:inherit font-lock-string-face))))
 '(nxml-attribute-value-face ((t (:inherit font-lock-string-face))))
 '(nxml-comment-content-face ((t (:inherit font-lock-comment-face))))
 '(nxml-comment-delimiter-face ((t (:inherit font-lock-comment-delimiter-face))))
 '(nxml-element-local-name-face ((t (:inherit font-lock-function-name-face))))
 '(nxml-processing-instruction-delimiter-face ((t nil)))
 '(nxml-processing-instruction-target-face ((t (:inherit font-lock-keyword-face))))
 '(nxml-tag-delimiter-face ((t nil)))
 '(rst-level-1-face ((t (:background "grey85" :foreground "black"))) t)
 '(rst-level-2-face ((t (:background "grey78" :foreground "black"))) t)
 '(tooltip ((((class color)) (:inherit variable-pitch :background "lightyellow" :foreground "black" :height 0.9))))
 '(variable-pitch ((t (:height 140 :family "helv")))))

;; Ubuntu Modes
(require 'python)
(require 'tramp)
(require 'dired)
(require 'dired-x)

;; emacs-goodies-el
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;; My Modes
(load "ledger")

(load "rst-mode")
(add-to-list 'auto-mode-alist '("\\.rst$" . rst-mode))

;; Load Relax NG autoloads
;; (load "rng-auto")

(require 'doctest-mode)
(require 'outdent)

(require 'dired-sort)

;; My own code
(require 'numbered-buffers) ;; needs lexical-let
(require 'single-dired)


;; X11 Selection configuration

(setq mouse-drag-copy-region nil)  ; stops selection with a mouse being immediately injected to the kill ring
(setq x-select-enable-primary nil)  ; stops killing/yanking interacting with primary X11 selection 
(setq x-select-enable-clipboard t)  ; makes killing/yanking interact with clipboard X11 selection

(setq select-active-regions t) ;  active region sets primary X11 selection
(global-set-key [mouse-2] 'mouse-yank-primary)  ; make mouse middle-click only paste from primary X11 selection, not clipboard and kill ring.

(cua-mode t)
(pc-selection-mode)
(delete-selection-mode t)

(require 'mouse-sel)

(require 'hippie-exp)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev-visible
        try-expand-dabbrev
        try-expand-line
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        ))

(autoload 'dabbrev-expand "dabbrev" "Word completion." t)
(autoload 'turn-on-lazy-lock "lazy-lock" "Force enable Lazy Lock mode.")

(setq next-line-add-newlines nil)

(setq lisp-indent-function #'common-lisp-indent-function)
(add-hook 'lisp-mode-hook
          (lambda ()
            (set (make-local-variable 'lisp-indent-function)
                 'common-lisp-indent-function)
            (modify-syntax-entry 58 "w")
            (local-set-key (kbd "<tab>")
                           'lisp-indent-or-complete)))

(setq-default scroll-up-aggressively 0.0)
(setq-default scroll-down-aggressively 0.0)
(setq scroll-step 1
      scroll-conservatively 100000)
(setq show-trailing-whitespace t)
(setq visible-bell t)

(put 'eval-expression 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq enable-recursive-minibuffers t)

(fset 'yes-or-no-p 'y-or-n-p)           ;replace y-e-s by y
(setq inhibit-startup-message t)        ;no splash screen
(defconst query-replace-highlight t)    ;highlight during query
(defconst search-highlight t)           ;highlight incremental search

(defconst ediff-ignore-similar-regions t)
(defconst ediff-use-last-dir t)
(defconst ediff-diff-options " -b ")

(add-hook 'dired-mode-hook
          '(lambda()
            (define-key dired-mode-map [delete] 'dired-do-delete)
            (define-key dired-mode-map [C-return] 'dired-find-file-other-window)
            (define-key dired-mode-map [C-down-mouse-1] 'mouse-buffer-menu)
            (define-key dired-mode-map [double-down-mouse-1] 'dired-mouse-find-file)
            (unless dired-omit-mode (dired-omit-mode))
            ))

;; Set the window title to show the opened file
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(column-number-mode t)                        ;column number in modeline (status)
(line-number-mode t)                        ;line number in modeline (status bar)

(defconst dabbrev-always-check-other-buffers t)
(defconst dabbrev-abbrev-char-regexp "\\sw\\|\\s_")


(show-paren-mode 1)

(global-set-key [(alt /)] 'dabbrev-expand)
(global-set-key [(control /)] 'hippie-expand)

(global-set-key [f4] 'kill-this-buffer)
(global-set-key [C-f4] 'server-edit)
(global-set-key [f6] 'other-window)
(global-set-key [f7] 'delete-other-windows)
(global-set-key [f11] 'comment-region)

(global-set-key [(meta g)] 'goto-line)

(defun insert-pdb (arg)
  "Insert pdb into code"
  (interactive "p")
  (beginning-of-line)
  (insert "import pdb; pdb.set_trace()\n")
  (previous-line)
  (indent-for-tab-command))
(global-set-key [(meta p)] 'insert-pdb)

(defun voblia/goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t nil)))

(global-set-key [(control \])] 'voblia/goto-match-paren)

(defun voblia/home ()
  "My own home function to act like med home"
  (interactive)
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)))

(global-set-key (kbd "<home>") #'voblia/home)
(put 'voblia/home 'CUA 'move)

(setq dired-omit-files-p t)
(defun voblia/dired-sort ()
  "Dired sort hook to list directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
  (set-buffer-modified-p nil))

(add-hook 'dired-after-readin-hook 'voblia/dired-sort)

(if (fboundp 'executable-make-buffer-file-executable-if-script-p)
    (add-hook 'after-save-hook
              'executable-make-buffer-file-executable-if-script-p))

(defun custom-indent-hook nil
  (save-excursion
    (move-to-column 0)
    (while
        (search-forward-regexp ",[^ ]"
                               (point-at-eol)
                               t)
      (search-backward-regexp ",[^ ]"
                              (point-at-bol))
      (if (not (eql (get-text-property (point) 'face)
                'font-lock-string-face))
       (replace-string "," ", " nil
                       (point)
                       (+ 1 (point)))
       (forward-char)))))

(defun indent-or-complete ()
  "Complete if point is at end of a word, otherwise indent line."
  (interactive)
  (if (looking-at "\\>")
      (dabbrev-expand nil)
      (indent-for-tab-command)
      ))

(defun with-filename (file-name action)
  (interactive)
  (with-temp-buffer
    (insert file-name)
    (goto-char (point-max))
    (funcall action)
    (when (file-exists-p (buffer-string))
      (buffer-string))))

(defun find-source-file-name ()
  (interactive)
  (let ((file-name (buffer-file-name)))
    (or
     (with-filename file-name
         (lambda ()
           (if (search-backward "/test_" nil t)
               (replace-match  "/")
               (insert "lalala"))))
     (with-filename file-name
         (lambda ()
           (if (search-backward  "/tests/test_" nil t)
               (replace-match "/")
               (insert "lalala"))))
     (with-filename file-name
         (lambda ()
           (if (search-backward  "/test/test_" nil t)
               (replace-match "/")
               (insert "lalala"))))
     (with-filename file-name
       (lambda ()
         (if (search-backward-regexp  "/tests/test_.*.py" nil t)
             (replace-match "/__init__.py")
             (insert "lalala")))))))

(defun find-test-file-name ()
  (interactive)
  (let ((file-name (buffer-file-name)))
    (or
     (with-filename file-name
       (lambda ()
         (when (search-backward  "/" nil t)
           (replace-match "/tests/test_"))))
     (with-filename file-name
       (lambda ()
         (when (search-backward  "/" nil t)
           (replace-match "/test/test_"))))
     (with-filename file-name
       (lambda ()
         (when (search-backward  "/" nil t)
           (replace-match "/test_"))))
     (with-filename file-name
       (lambda ()
         (if (search-backward  "tests.py" nil t)
             (insert "lalala")
             (progn
               (search-backward  "/" nil t)
               (delete-region (point) (line-end-position))
               (insert "/tests.py")))))
     (with-filename file-name
       (lambda ()
         (if (search-backward-regexp "/\\(.*?\\)/__init__.py" nil t)
             (replace-match "/\\1/tests/test_\\1.py")
             (insert "lalal")))))))

(defun switch-between-source-and-test ()
  (interactive)
  (let ((source-file (find-source-file-name))
        (test-file (find-test-file-name)))
    (cond (test-file (find-file test-file))
          (source-file (find-file source-file))
          (t (switch-to-buffer nil)))))

(defun smart-recompile ()
  (interactive)
  (let ((target-buffer-name (regexp-find-buffer "*compilation*")))
    (when target-buffer-name
      (save-window-excursion
        (switch-to-buffer "*compilation*")
        (let ((comp-proc (get-buffer-process (current-buffer))))
          (when comp-proc
            (condition-case ()
                (progn
                  (interrupt-process comp-proc)
                  (sit-for 1)
                  (delete-process comp-proc))
              (error nil))))
        (rename-buffer (format "*compilation %s*" (format-time-string "%H:%M:%S")))))
    (when (and (> (length  (buffer-name)) 12)
               (equal (substring (buffer-name) 0 12)
                      "*compilation"))
      (switch-to-buffer "*compilation*"))
    (recompile)))

(add-hook 'c-mode-common-hook
          (function (lambda ()
            (local-set-key (kbd "<tab>") 'indent-or-complete))))

(defun compilation-buffers ()
  (sort
   (loop for buffer in (buffer-list)
      when (and (> (length  (buffer-name buffer)) 12)
                (equal (substring (buffer-name buffer) 0 12)
                       "*compilation"))
      collect buffer)
   (lambda (a b)
     (string-lessp (buffer-name a)
                   (buffer-name b)))))

(defun next-compilation-buffer ()
  (interactive)
  (let ((compilation-buffers (compilation-buffers)))
    (unless (equal (buffer-name (current-buffer))
                   (buffer-name (car (last compilation-buffers))))
      (let ((buffer-number
             (position (current-buffer)
                compilation-buffers :test (lambda (a b) (equal (buffer-name a)
                                                               (buffer-name b))))))
        (switch-to-buffer (nth (+ 1 buffer-number)
                               compilation-buffers))))))


(defun previous-compilation-buffer ()
  (interactive)
  (let ((compilation-buffers (reverse (compilation-buffers))))
    (unless (equal (buffer-name (current-buffer))
                   (buffer-name (car (last compilation-buffers))))
      (let ((buffer-number
             (position (current-buffer)
                compilation-buffers :test (lambda (a b) (equal (buffer-name a)
                                                               (buffer-name b))))))
        (switch-to-buffer (nth (+ 1 buffer-number)
                               compilation-buffers))))))

(add-hook 'compilation-mode-hook
          (function (lambda ()
            (local-set-key (kbd "n") 'next-compilation-buffer)
            (local-set-key (kbd "p") 'previous-compilation-buffer)
            (local-set-key (kbd "k") 'kill-this-buffer))))

;; (add-hook 'font-lock-mode-hook
;;           (function
;;            (lambda ()
;;             (setq font-lock-keywords
;;                   (append font-lock-keywords
;;                           '(("\t+" (0 'my-tab-face t))))))))


(defun find-tag-again ()
  (interactive)
  (find-tag "" t))


(setq auto-mode-alist
      (append auto-mode-alist
             '(("\\.py\\'" . python-mode)
               ("\\.po[tx]?\\'\\|\\.po\\." . po-mode)
               ("\\.zcml\\'" . nxml-mode)
               ("\\.dtml\\'" . sgml-mode)
               ("\\.rng\\'" . nxml-mode)
               ("\\.z?pt\\'" . html-mode)
               ("\\.mako\\'" . html-mode)
               ("\\.txt$\\'" . org-mode)
               )))

(global-set-key [C-f9] 'pigide-test)
(global-set-key [f9] 'smart-recompile)
(add-hook 'python-mode-hook
          (function (lambda ()
            (hs-minor-mode)
            (flymake-mode)
            (local-set-key [C-c C-c] 'recompile))))

(add-hook 'html-mode-hook
          (function (lambda ()
            (hs-minor-mode))))

(global-set-key [C-f10] 'pigide-compile)
(global-set-key [f10] 'smart-recompile)

(defun send-to-gvim ()
  (interactive)
  (when (buffer-file-name)
    (shell-command (format "gvim --remote %s" (buffer-file-name)))))

(server-start)
(find-file "~/")
(rename-buffer "*1")

(setq *last-highlighted-block* nil)
(defun toggle-higlight-line ()
  (interactive)
  (if *last-highlighted-block*
      (progn
        (font-lock-remove-keywords nil (list *last-highlighted-block*))
        (setf *last-highlighted-block* nil))
      (progn
        (let ((lock-block (list (format "^[^\n]\\{%d\\}\\(.\\)" (current-column)) '(1 'my-long-line-face append))))
          (setf *last-highlighted-block* lock-block)
          (font-lock-add-keywords nil (list *last-highlighted-block*)))))
  (font-lock-mode t))

(global-set-key [f12] 'toggle-higlight-line)

(font-lock-add-keywords
 'python-mode
 '(("\t+" (0 'my-tab-face append))
   ("^[^\n]\\{80\\}\\(.*\\)$"
    1 'my-long-line-face append)))

(font-lock-add-keywords
 'emacs-lisp-mode
 '(("\t+" (0 'my-tab-face append))
   ("^[^\n]\\{80\\}\\(.*\\)$"
    1 'my-long-line-face append)))

(font-lock-add-keywords
 'lisp-mode
 '(("\t+" (0 'my-tab-face append))
   ("^[^\n]\\{80\\}\\(.*\\)$"
    1 'my-long-line-face append)))

(font-lock-add-keywords
 'text-mode
 '(("\t+" (0 'my-tab-face append))
   ("^[^\n]\\{80\\}\\(.*\\)$"
    1 'my-long-line-face append)))

;(setq tagging-tagline-indicator "^#\\*")
;(setq tagging-tagline-indicator " ?\(def\)\|\(class\)") ;taging for tests

(setq outline-blank-line t)

(defun kill-compilation-buffers ()
  (interactive)
  (save-excursion
    (loop for b in (buffer-list)
       when (and (string-match "\\*compilation" (buffer-name b))
                 (not (string-match "\\*compilation\\*" (buffer-name b))))
       do (kill-buffer b))))
(global-set-key [M-f9] 'kill-compilation-buffers)

(defun parse-int ()
  (interactive)
  (let ((sum 0))
    (loop for c across (buffer-substring (region-beginning) (region-end))
       do (setf sum (+ (* sum 256) c)))
    (message (format "%d" sum))))

(defconst py-blank-or-comment-re "[ \t]*\\($\\|#\\)"
  "Regular expression matching a blank or comment line.")

(defconst py-blank-re "[ \t]*\\($\\)"
  "Regular expression matching a blank line.")

(defun python-hs-forward-sexp-func (&optional count)
  (interactive)
  (let ((pos (point)))
    (beginning-of-line)
    (cond
     ((looking-at ".*") ; returns true
      (setq bi (current-indentation)) ; record current indentation
      (forward-line 1)                ; go to next line
      (while (and
              (or
               (looking-at py-blank-or-comment-re) ; find next dedented line
               (< bi (current-indentation)))
              (= (forward-line 1) 0))
        nil
        )
      ; now back up
      (forward-line -1)
      (while (and (looking-at py-blank-re)
                  (= (forward-line -1) 0))
        nil))
     (t (goto-char pos)))))

(setq hs-special-modes-alist
      (cons '(python-mode
              "\\s-*\\(def.*\\|class.*\\|if.*\\|else.*\\|elif.*\\|try.*\\|except.*\\|finally.*\\)\\(:\\|$\\)"
              nil
              nil
              python-hs-forward-sexp-func
              nil) hs-special-modes-alist))

(setq hs-special-modes-alist
      (cons '(html-mode
              "<.*"
              nil
              nil
              sgml-skip-tag-forward
              nil) hs-special-modes-alist))

(global-set-key "\M-\r" 'hs-toggle-hiding)

(add-to-list 'tramp-default-proxies-alist
             '("\\`schooltool\\.org\\'" "\\`buildbot\\'" "/ssh:%h:"))

(require 'pigide)
(setq *pigide-active-project* "/home/ignas/src/ututi/ututi")
(pigide-setup)

(require 'py-imports)
(setq py-import-interactive-select-tag nil)
(setq py-import-find-all-locations-of-tag 'find-all-locations-of-tag-vtags)

(defun pyflake ()
  (interactive)
  (compile (format "pyflakes %s" (buffer-file-name))))

(global-set-key [f8] 'pyflake)
(global-set-key [f1] 'find-file-mouse)
(global-set-key [C-f6] 'switch-between-source-and-test)
(global-set-key [f5] 'select-tag-to-import-at-top)
(global-set-key [S-f5] 'select-tag-to-import-in-place)
(global-set-key [C-f5] 'select-tag-to-expand)

(setq py-indent-offset 4)
(setq indent-tabs-mode nil)

(add-hook 'text-mode-hook
          '(lambda () (setq tab-width 4)))

(add-hook 'python-mode-hook
          (function (lambda ()
            (local-set-key (kbd "<tab>") 'indent-or-complete))))

(defun find-file-mouse ()
  (interactive)
  (let ((file-name (x-get-selection 'PRIMARY)))
    (when (and file-name
               (not (string= file-name "")))
      (loop for prefix in '(""
                            "/home/ignas/src/schooltool/schooltool_date_management/")
           when (file-exists-p (format "%s%s" prefix
                                       file-name))
           do (find-file (format "%s%s" prefix
                                 file-name))))))
