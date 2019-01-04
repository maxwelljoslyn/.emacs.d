;; -*- coding: utf-8; lexical-binding: t; -*-


;; use a macro which does C-s ":type" C-s ?{ expand-region, then calls this function
(defun mj/update-coord (arg1 arg2)
  (interactive "r")
  (save-restriction
    (narrow-to-region arg1 arg2)
    (while (re-search-forward "[0-9]\\{2,3\\}" nil t)
      (goto-char (match-beginning 0))
      (replace-match
       (number-to-string (* 2.08 (string-to-number (match-string 0))))))))

(defun mj/video-id ()
  "Get the ID of the cut video on the current line."
  (save-excursion
    (end-of-line)
    (buffer-substring (point) (- (point) 2))))

;; to adapt this setup to move from e.g. RS to EV:
;; 1) change RS-cut within mj/open-cut-video's `concat` to EV-cut
;; 2) change code in cut_vids.py to only regenerate EV files
(defun mj/open-cut-video ()
  "Open the cut video created from the timestamps on the current line."
  (interactive)
  (save-buffer)
  (save-excursion
    (let* ((video-path (concat "/Users/joslynm/Box Sync/vanSanten-R01-ASR/AVALA_prompt_videos/FS-cut-"
                               (mj/video-id)
                               ".mov")))
      (mj/cut-vid)
      (async-shell-command
       (concat "open " (shell-quote-argument video-path))))))

(defun mj/cut-vid ()
  "Run the video-trimming shell script on the video referenced on the current line. "
  (let ((vi (mj/video-id)))
    (shell-command
     (concat "python3 " (shell-quote-argument "/Users/joslynm/Box Sync/vanSanten-R01-ASR/AVALA_prompt_videos/cut_vids.py") " -f " vi " -t " vi))))
(defun mj/open-cslu-dir ()
  "Runs Dired in one of the directories I often visit on my work machine."
  (interactive)
  (if-let (a-dir
           (helm :sources (helm-build-sync-source "Directories"
                            :candidates '("/Users/joslynm/Box Sync/vanSanten-R01-ASR/"
                                          "/Users/joslynm/Desktop/AVALA/"))
                 :buffer "*helm cslu directories*"))
      (dired a-dir)
    (error "No directory selected")))
(defun mj/meeting-template ()
  (let ((date-string (org-read-date nil nil ".")))
    (setq  template "* NEXT AVALA meeting minutes %(org-insert-time-stamp (org-read-date nil t \".\"))\n  :PROPERTIES:\n  :export_file_name: AVALA_minutes_")
    (setq export-date (replace-regexp-in-string "-" "_" date-string))
    (setq template (s-concat template export-date "\n  :END:\n** Discussion\n** Actions"))
    template))


(defun mj/copy-csv-field (source-buf &optional linum field-num)
  "Get the CSV field at the LINUM line and FIELD-NUM field in the buffer SOURCE-BUF, which must be visiting a CSV file."
  (interactive)
  (when (not linum)
    (setq linum 1))
  (when (not field-num)
    (setq field-num 1))
  (with-current-buffer source-buf
    (goto-line linum)
    (beginning-of-line)
    (csv-forward-field field-num)
    (setq end (1+ (point)))
    (setq beg  (if-let (posn (search-backward "," (line-beginning-position) t))
                   (1+  posn)
                 (line-beginning-position)))
    (buffer-substring-no-properties beg end)))

(defun mj/csv-column-to-row (rows cols source-buf target-buf)
  "Given a buffer SOURCE-BUF with variables as columns and observations in rows, insert into TARGET-BUF a row with the variable as the first column and the observations as subsequent columns."
  (with-current-buffer target-buf
    (dolist (i (number-sequence 1 rows))
      (setq text (mj/copy-csv-field source-buf i cols))
      (goto-line (1+ cols))
      (end-of-line)
      (insert text))
    (insert "\n")))

(defun mj/toggle-Redcap-is-identifier ()
    "Toggle whether a Redcap field is an identifier or not."
    (interactive)
    (save-excursion
      (end-of-line)
      (search-backward "," nil t 7)
      (backward-char)
      (if (looking-at "y")
          (delete-char 1)
        (progn
          (forward-char)
          (insert "y")))))
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
