;; But they both do not support dired-up-directory, and when you go up
;; the directory tree they put you in the top of the directory list
;; (not like default dired that puts you on the directory name you
;; were in) so there is one more way to achieve this.
(eval-after-load "dired"
  '(progn
    (defadvice dired-advertised-find-file (around dired-subst-directory activate)
     "Replace current buffer if file is a directory."
     (interactive)
     (let* ((orig (buffer-name (current-buffer)))
            (filename (dired-get-filename nil t))
            (bye-p (file-directory-p filename))
            (current-dir (dired-current-directory)))

       ad-do-it

       (when (and bye-p (not (string-match "[/\\\\]\\.$" filename)))
         ;; buffer keeps it's original name
         (kill-buffer orig)
         (rename-buffer orig)
         ;; if we went up the directory tree we should go to the directory name
         ;; we were before
         (when (string-match "\\.\\.$" filename)
           (let ((begining (string-match "/[^/]*/$" current-dir)))
             (when begining
               ;; go to the name of the subdir
               ;; isn't perfect if you have directory names with spaces in them
               (re-search-forward (concat " " (substring current-dir (+ 1 begining) -1) "$")    nil t)
               ;; workaround so the cursor would apear in the begining of the line
               ;; because adter re-search-forward it appears in the end of the line
               (dired-previous-line 1)
               (dired-next-line 1)))))))))

(defun my-dired-go-up ()
  (interactive)
  (if (or (re-search-backward " \\.\\.$" nil t)
          (re-search-forward " \\.\\.$" nil t))
      (dired-advertised-find-file)
      (progn (dired-omit-toggle)
             (or (re-search-backward " \\.\\.$" nil t)
                 (re-search-forward " \\.\\.$" nil t))
             (dired-advertised-find-file))))

(define-key dired-mode-map  (kbd "<left>") 'my-dired-go-up)
(define-key dired-mode-map  (kbd "<right>") 'dired-advertised-find-file)
(provide 'single-dired)
