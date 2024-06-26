;;; -*- lexical-binding: t; -*-

;; A Simple minor mode to walk through every file in a project
(require 'fileloop)

(defvar zimmerframe-buffer "*Project Zimmerframe*")

(defvar zimmerframe-root-text "# Root: ")

(defun zimmerframe-init ()
  "Initialize a walker in the project root"
  (interactive)
  (let* ((current (projectile-project-root))
         (files (projectile-project-files current)))
    (with-current-buffer (get-buffer-create zimmerframe-buffer)
      (setq-local doom-real-buffer-p t)
      (erase-buffer)
      (insert zimmerframe-root-text current "\n")
      (mapc (lambda (x) (insert x "\n")) files)
      (goto-char (point-min))
      (forward-line 1)
      )
    (message "Initialized Zimmerframe with root: %s" current)
    )
  )

(defun zimmerframe-directory-init ()
  "Init a walker in a specific directory"
  (interactive)
  (let* ((current (read-directory-name "Starting Point: "))
         (files (projectile-project-files current)))
    (with-current-buffer (get-buffer-create zimmerframe-buffer)
      (erase-buffer)
      (insert zimmerframe-root-text current "\n")
      (mapc (lambda (x) (insert x "\n")) files)
      (goto-char (point-min))
      (forward-line 1)
      )
    )
  )

(defun zimmerframe-filter-defaults ()
  "User defaults to filter the walker"
  (interactive)
  (with-current-buffer zimmerframe-buffer
    (goto-char (point-min))
    (forward-line 1)
    (flush-lines zimmerframe-filter-default-regexp)
    (goto-char (point-min))
    (forward-line 1)
    (flush-lines (string-join zimmerframe-filter-default-exclusions "\\"))
    )
  (zimmerframe-remaining-count)
)

(defun zimmerframe-filter-keep (arg)
  "Keep only files matching a regex in the walker"
  (interactive "MKeep filenames: ")
  (with-current-buffer zimmerframe-buffer
    (goto-char (point-min))
    (forward-line 1)
    (keep-lines arg)
    )
  )

(defun zimmerframe-filter (arg)
  "Filter a regex from the walker"
  (interactive "MRegexp to filter: ")
  (with-current-buffer zimmerframe-buffer
    (goto-char (point-min))
    (forward-line 1)
    (flush-lines arg)
    )
)

(defun zimmerframe--root ()
  "Get the root of the walker"
  (with-current-buffer zimmerframe-buffer
    (goto-char (point-max))
    (when (re-search-backward zimmerframe-root-text nil t)
      (buffer-substring (+ (point) (length zimmerframe-root-text))
                        (line-end-position)))))

(defun zimmerframe--find (file)
  "Find a file in the walker"
  (let ((root (zimmerframe--root)))
    (cond ((f-dir? file)
           (message "Entering Directory: %s" file))
          ((and (s-matches? "^/" file) (f-exists? file))
           (find-file file))
          ((and root (f-exists? (f-join root file)))
           (find-file (f-join root file)))
          (t (message "Doesn't exist: %s %s" file root))
          )
    )
  )

(defun zimmerframe-next ()
  " Go to the next buffer in 'zimmerframe-buffer
going by the root (signified by (rx bol #))
or by the last visited file (signified by (rx bol *))
 "
  (interactive)
  (cond ((null (get-buffer zimmerframe-buffer))
         (message "Project Walk Not Started"))
        (t (with-current-buffer zimmerframe-buffer
             (goto-char (point-max))
             (if (re-search-backward "^* " nil t)
                 (forward-line 1)
               (goto-char (point-min)))
             (insert "* ")
             (cond ((looking-at "^*") nil)
                   (t (zimmerframe--find (buffer-substring (point) (line-end-position))))
                   ))
           )
        )
  )

(defun zimmerframe-prev ()
  "Get the Previous file in the walker"
  (interactive)
  (cond ((null (get-buffer zimmerframe-buffer))
         (message "Project Walk Not Started"))
        (t (with-current-buffer zimmerframe-buffer
             (goto-char (point-max))
             (when (re-search-backward "^* " nil t)
               (delete-region (point) (+ (point) 2))
               )
             (cond ((looking-at "^*") nil)
                   (t (zimmerframe--find (buffer-substring (point) (line-end-position))))
             )))
        )
  )

(defun zimmerframe-remaining ()
  (interactive)
  (cond ((null (get-buffer zimmerframe-buffer))
         (message "Project Walk Not Started"))
        ((with-current-buffer zimmerframe-buffer
           (goto-char (point-max))
           (re-search-backward "^\\(*\\|#\\) ")
           (forward-line)
           (not (< (point) (point-max))))
         (display-buffer zimmerframe-buffer)
         (message "Project Walk Completed"))
        ;; ((intern-soft "+popup-buffer")
        ;;  (message "Popping")
        ;;  (+popup-buffer (get-buffer zimmerframe-buffer)))
        (t
         (message "Displaying")
         (display-buffer zimmerframe-buffer))
        )
  )

(defun zimmerframe-remaining-count ()
  (cond ((null (get-buffer zimmerframe-buffer))
         -1)
        (t
         (with-current-buffer zimmerframe-buffer
           (goto-char (point-max))
           (if (re-search-backward "^* " nil t)
               (forward-line 1)
             (goto-char (point-min)))
           (count-lines (point) (point-max))))
        ))

(defun zimmerframe-num ()
  (interactive)
  (cond ((null (get-buffer zimmerframe-buffer))
         (message "Project Walk Not Started"))
        ((with-current-buffer zimmerframe-buffer
           (goto-char (point-max))
           (re-search-backward "^\\(*\\|#\\) ")
           (forward-line)
           (not (< (point) (point-max))))
         (message "Project Walk Completed"))
        (t
         (with-current-buffer zimmerframe-buffer
           (let ((lineno (list (progn (goto-char (point-max))
                                      (re-search-backward "^\\(*\\|#\\) ")
                                      (forward-line 1)
                                      (line-number-at-pos))
                               (progn (goto-char (point-max))
                                      (line-number-at-pos)))))
             (message "Remaining to Walk: %s/%s" (1- (car lineno)) (1- (cadr lineno)))
             )
           )
         )
        )
  )

(defun zimmerframe--replace-scan (mstart from)
  " The scan function for fileloop "
  (when (re-search-forward from nil t)
    ;; When we find a match, save its beginning for
    ;; `perform-replace' (we used to just set point, but this
    ;; is unreliable in the face of
    ;; `switch-to-buffer-preserve-window-point').
    (puthash (current-buffer) (match-beginning 0) mstart)
    )
)

(defun zimmerframe--replace-operate (mstart from to)
  " the operate function for fileloop"
  (vimish-fold-unfold-all)
  (perform-replace from to t t nil nil multi-query-replace-map
                   (gethash (current-buffer) mstart (point-min))
                   (point-max))
  )

(iter-defun zimmerframe--iter-next ()
  "An iterator that returns the next buffer to walk to"
  (interactive)
  (let ((root (zimmerframe--root))
        (z-point 0)
        (z-max (with-current-buffer zimmerframe-buffer
                 (goto-char (point-min))
                 (point-max)))
        current
        )
    (while (< z-point z-max)
      (with-current-buffer zimmerframe-buffer
        (while (and (not (eobp)) (looking-at "^\\(#\\|\*\\)" t))
          (forward-line 1)
          )
        (setq current (f-join root (buffer-substring (point) (line-end-position))))
        (insert "* ")
        (forward-line 1)
        (setq z-point (point))
        )
      (when (and (f-file? current) (f-exists? current))
        (iter-yield current)
        )
      )
    )
  )

(defun zimmerframe-replace-regexp ()
  "Use zimmerframe to walk the project and replace a regexp"
  (interactive)
  (unless (get-buffer zimmerframe-buffer)
    (zimmerframe-init)
    )
  (let* ((regexp (read-string "Replace Regexp: "))
         (target (read-string (format "Replace %s with: " regexp)))
         (mstart (make-hash-table :test 'eq))
         (case-fold-search nil)
         )
    (fileloop-initialize
     (funcall #'zimmerframe--iter-next)
     (-partial #'zimmerframe--replace-scan mstart regexp)
     (-partial #'zimmerframe--replace-operate mstart regexp target)
     )
    (fileloop-continue)
    )
  )

;;;###autoload
(define-minor-mode project-zimmerframe-minor-mode
  " A minor mode to walk through all "
  :lighter "Zimmerframe"
  :global t
  (setq-default zimmerframe-filter-default-exclusions '("__init__.py")
                zimmerframe-filter-default-regexp (rx line-start ?.)
                )
  (cond (project-zimmerframe-minor-mode
         (zimmerframe-init))
        ((get-buffer zimmerframe-buffer)
         (kill-some-buffers (list (get-buffer zimmerframe-buffer))))
        (t nil)
    )
)

(provide 'project-zimmerframe)
