(defvar dired-sort-map (make-sparse-keymap))

(add-hook 'dired-mode-hook '(lambda () (define-key dired-mode-map "s" dired-sort-map)))
(add-hook 'dired-mode-hook '(lambda () (define-key dired-sort-map "s" '(lambda () "sort by Size" (interactive) (dired-sort-other (concat dired-listing-switches "S"))))))
(add-hook 'dired-mode-hook '(lambda () (define-key dired-sort-map "x" '(lambda () "sort by eXtension" (interactive) (dired-sort-other (concat dired-listing-switches "X"))))))
(add-hook 'dired-mode-hook '(lambda () (define-key dired-sort-map "t" '(lambda () "sort by Time" (interactive) (dired-sort-other (concat dired-listing-switches "t"))))))
(add-hook 'dired-mode-hook '(lambda () (define-key dired-sort-map "n" '(lambda () "sort by Name" (interactive) (dired-sort-other (concat dired-listing-switches ""))))))
(provide 'dired-sort)
