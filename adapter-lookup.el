;; Experimental code for emacs integration with ZCA
;; A barely working prototybe, but might inspire someone

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
