(defun mj/toggle-Redcap-required ()
    "Toggle whether a line (Redcap 'field') is required or not."
    (interactive)
    (save-excursion
      (end-of-line)
      (search-backward "," nil t 5)
      (backward-char)
      (if (looking-at "y")
          (delete-char 1)
        (progn
          (forward-char)
          (insert "y")))))

(defun mj/digitization-tasks (arg)
  "Create 'org-mode' digitization tasks for video ARG."
  (interactive "sVideotape ID code:")
  (save-excursion
    (set-buffer "todo.org")
    (org-insert-heading)
    (insert (concat "tape " arg ": begin digitizing"))
    (org-todo "NEXT")
    (org-insert-heading)
    (insert (concat "tape " arg ": clean up digitization"))
    (org-todo "TODO")
    (org-insert-heading)
    (insert (concat "tape " arg ": begin exporting"))
    (org-todo "TODO")
    (org-insert-heading)
    (insert (concat "tape " arg ": upload to server"))
    (org-todo "TODO")))


(defun mj/replace-Xs (arg)
  "Replace Xs with ARG."
  (interactive "sReplace Xs with:")
  (mark-paragraph)
  (vr/replace "X" arg (point) (mark))
  (next-line 2))


;; these are a bit buggy. they work correctly, but only sometimes
;; I don't need them anymore right now, so I guess it's moot... I'll write more functions in my life
;; (defun buggy/rejected-filename-to-rejects ()
;;   "Move the file number of a rejected audio file to the corresponding speaker-specific reject.txt file. The lines on which this function operates look like this: ./dat/speakers/A-10/1.wav,bad,static"
;;   (interactive)
;;   (search-forward "wav")
;;   (backward-word)
;;   (backward-char)
;;   (let ((file-number (buffer-substring (point) (progn (search-backward "/")
;;                                                       (forward-char)
;;                                                       (point)))))
;;     (beginning-of-line)
;;     (let ((rejects-file
;;            (concat
;;             (buffer-substring (point)
;;                               (search-forward "/" nil t 4))
;;             "reject.txt")))
;;       (find-file rejects-file)
;;       (goto-char (point-max))
;;       (insert file-number)
;;       (save-buffer)
;;       (kill-buffer))))

;; (defun buggy/remove-files-I-kept-Alex-rejected ()
;;   "The lines on which this function operates look like this: ./dat/speakers/A-10/1.wav,good,sounds good to me this is a comment"
;;   (interactive)
;;   (search-forward "wav")
;;   (backward-word)
;;   (backward-char)
;;   (let ((file-number (buffer-substring (point) (progn (search-backward "/")
;;                                                       (forward-char)
;;                                                       (point)))))
;;     (beginning-of-line)
;;     (let ((rejects-file
;;            (concat
;;             (buffer-substring (point)
;;                               (search-forward "/" nil t 4))
;;             "reject.txt")))
;;       (find-file rejects-file)
;;       (goto-char (point-min))
;;       (when (word-search-forward file-number nil t)
;;         (beginning-of-line)
;;         (kill-whole-line)
;;         (save-buffer))
;;       (kill-buffer))))