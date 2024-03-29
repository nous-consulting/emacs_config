(eval-when-compile
  (require 'cl))

(defun get-non-system-buffer ()
  (loop for b in (buffer-list)
        when (not (string-match "\\*" (buffer-name b)))
        return (buffer-name b)))

(defun regexp-find-buffer (buffer-regexp)
  (loop for b in (buffer-list)
       when (string-match buffer-regexp (buffer-name b))
       return (buffer-name b)))

(defun generic-switch (buffer-number)
  (interactive)
  (let* ((buffer-name (format "*%s" buffer-number))
         (buffer-regexp (format "\\%s" buffer-name))
         (target-buffer-name (regexp-find-buffer buffer-regexp)))
    (if target-buffer-name
        (switch-to-buffer target-buffer-name)
        (let ((non-system-buffer (get-non-system-buffer)))
          (when non-system-buffer
            (switch-to-buffer non-system-buffer)
            (set-buffer non-system-buffer)
            (rename-buffer (if (buffer-file-name)
                               (format "%s %s" buffer-name (buffer-file-name))
                               buffer-name)))))))

(dotimes (i 10)
  (lexical-let ((j i))
    (global-set-key (eval `(kbd ,(format "M-%s" j)))
                    (lambda ()
                      (interactive)
                      (generic-switch j)))))
(provide 'numbered-buffers)
