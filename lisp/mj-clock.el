(defun mj/org-clock-in (&optional select start-time)
  "Start the clock on the current item.
If necessary, clock-out of the currently active clock.
With a prefix argument SELECT (\\[universal-argument]), offer a list of recently clocked
tasks to clock into.  When SELECT is \\[universal-argument] \\[universal-argument], clock into the current task
and mark it as the default task, a special task that will always be offered
in the clocking selection, associated with the letter `d'.
When SELECT is \\[universal-argument] \\[universal-argument] \\[universal-argument], \
clock in by using the last clock-out
time as the start time \(see `org-clock-continuously' to
make this the default behavior.)"
  (interactive "P")
  (setq org-clock-notification-was-shown nil)
  (org-refresh-properties org-effort-property 'org-effort)
  (catch 'abort
    (let ((interrupting (and (not org-clock-resolving-clocks-due-to-idleness)
			     (org-clocking-p)))
	  ts selected-task target-pos (org--msg-extra "")
	  (leftover (and (not org-clock-resolving-clocks)
			 org-clock-leftover-time)))

      (when (and org-clock-auto-clock-resolution
		 (or (not interrupting)
		     (eq t org-clock-auto-clock-resolution))
		 (not org-clock-clocking-in)
		 (not org-clock-resolving-clocks))
	(setq org-clock-leftover-time nil)
	(let ((org-clock-clocking-in t))
	  (org-resolve-clocks)))	; check if any clocks are dangling

      (when (equal select '(64))
	;; Set start-time to `org-clock-out-time'
	(let ((org-clock-continuously t))
	  (org-clock-in nil org-clock-out-time)))

      (when (equal select '(4))
	(setq selected-task (mj/org-clock-select-task "Clock-in on task: "))
	(if selected-task
	    (setq selected-task (copy-marker selected-task))
	  (error "Abort")))

      (when (equal select '(16))
	;; Mark as default clocking task
	(org-clock-mark-default-task))

      (when interrupting
	;; We are interrupting the clocking of a different task.
	;; Save a marker to this task, so that we can go back.
	;; First check if we are trying to clock into the same task!
	(when (save-excursion
		(unless selected-task
		  (org-back-to-heading t))
		(and (equal (marker-buffer org-clock-hd-marker)
			    (if selected-task
				(marker-buffer selected-task)
			      (current-buffer)))
		     (= (marker-position org-clock-hd-marker)
			(if selected-task
			    (marker-position selected-task)
			  (point)))
		     (equal org-clock-current-task (nth 4 (org-heading-components)))))
	  (message "Clock continues in \"%s\"" org-clock-heading)
	  (throw 'abort nil))
	(move-marker org-clock-interrupted-task
		     (marker-position org-clock-marker)
		     (marker-buffer org-clock-marker))
	(let ((org-clock-clocking-in t))
	  (org-clock-out nil t)))

      ;; Clock in at which position?
      (setq target-pos
	    (if (and (eobp) (not (org-at-heading-p)))
		(point-at-bol 0)
	      (point)))
      (save-excursion
	(when (and selected-task (marker-buffer selected-task))
	  ;; There is a selected task, move to the correct buffer
	  ;; and set the new target position.
	  (set-buffer (org-base-buffer (marker-buffer selected-task)))
	  (setq target-pos (marker-position selected-task))
	  (move-marker selected-task nil))
	(save-excursion
	  (save-restriction
	    (widen)
	    (goto-char target-pos)
	    (org-back-to-heading t)
	    (or interrupting (move-marker org-clock-interrupted-task nil))
	    (run-hooks 'org-clock-in-prepare-hook)
	    (org-clock-history-push)
	    (setq org-clock-current-task (nth 4 (org-heading-components)))
	    (cond ((functionp org-clock-in-switch-to-state)
		   (looking-at org-complex-heading-regexp)
		   (let ((newstate (funcall org-clock-in-switch-to-state
					    (match-string 2))))
		     (if newstate (org-todo newstate))))
		  ((and org-clock-in-switch-to-state
			(not (looking-at (concat org-outline-regexp "[ \t]*"
						 org-clock-in-switch-to-state
						 "\\>"))))
		   (org-todo org-clock-in-switch-to-state)))
	    (setq org-clock-heading
		  (cond ((and org-clock-heading-function
			      (functionp org-clock-heading-function))
			 (funcall org-clock-heading-function))
			((nth 4 (org-heading-components))
			 (replace-regexp-in-string
			  "\\[\\[.*?\\]\\[\\(.*?\\)\\]\\]" "\\1"
			  (match-string-no-properties 4)))
			(t "???")))
	    (org-clock-find-position org-clock-in-resume)
	    (cond
	     ((and org-clock-in-resume
		   (looking-at
		    (concat "^[ \t]*" org-clock-string
			    " \\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}"
			    " *\\sw+.? +[012][0-9]:[0-5][0-9]\\)\\][ \t]*$")))
	      (message "Matched %s" (match-string 1))
	      (setq ts (concat "[" (match-string 1) "]"))
	      (goto-char (match-end 1))
	      (setq org-clock-start-time
		    (apply 'encode-time
			   (org-parse-time-string (match-string 1))))
	      (setq org-clock-effort (org-entry-get (point) org-effort-property))
	      (setq org-clock-total-time (org-clock-sum-current-item
					  (org-clock-get-sum-start))))
	     ((eq org-clock-in-resume 'auto-restart)
	      ;; called from org-clock-load during startup,
	      ;; do not interrupt, but warn!
	      (message "Cannot restart clock because task does not contain unfinished clock")
	      (ding)
	      (sit-for 2)
	      (throw 'abort nil))
	     (t
	      (insert-before-markers "\n")
	      (backward-char 1)
	      (org-indent-line)
	      (when (and (save-excursion
			   (end-of-line 0)
			   (org-in-item-p)))
		(beginning-of-line 1)
		(org-indent-line-to (- (org-get-indentation) 2)))
	      (insert org-clock-string " ")
	      (setq org-clock-effort (org-entry-get (point) org-effort-property))
	      (setq org-clock-total-time (org-clock-sum-current-item
					  (org-clock-get-sum-start)))
	      (setq org-clock-start-time
		    (or (and org-clock-continuously org-clock-out-time)
			(and leftover
			     (y-or-n-p
			      (format
			       "You stopped another clock %d mins ago; start this one from then? "
			       (/ (- (org-float-time
				      (org-current-time org-clock-rounding-minutes t))
				     (org-float-time leftover)) 60)))
			     leftover)
			start-time
			(org-current-time org-clock-rounding-minutes t)))
	      (setq ts (org-insert-time-stamp org-clock-start-time
					      'with-hm 'inactive))))
	    (move-marker org-clock-marker (point) (buffer-base-buffer))
	    (move-marker org-clock-hd-marker
			 (save-excursion (org-back-to-heading t) (point))
			 (buffer-base-buffer))
	    (setq org-clock-has-been-used t)
	    ;; add to mode line
	    (when (or (eq org-clock-clocked-in-display 'mode-line)
		      (eq org-clock-clocked-in-display 'both))
	      (or global-mode-string (setq global-mode-string '("")))
	      (or (memq 'org-mode-line-string global-mode-string)
		  (setq global-mode-string
			(append global-mode-string '(org-mode-line-string)))))
	    ;; add to frame title
	    (when (or (eq org-clock-clocked-in-display 'frame-title)
		      (eq org-clock-clocked-in-display 'both))
	      (setq frame-title-format org-clock-frame-title-format))
	    (org-clock-update-mode-line)
	    (when org-clock-mode-line-timer
	      (cancel-timer org-clock-mode-line-timer)
	      (setq org-clock-mode-line-timer nil))
	    (when org-clock-clocked-in-display
	      (setq org-clock-mode-line-timer
		    (run-with-timer org-clock-update-period
				    org-clock-update-period
				    'org-clock-update-mode-line)))
	    (when org-clock-idle-timer
	      (cancel-timer org-clock-idle-timer)
	      (setq org-clock-idle-timer nil))
	    (setq org-clock-idle-timer
		  (run-with-timer 60 60 'org-resolve-clocks-if-idle))
	    (message "Clock starts at %s - %s" ts org--msg-extra)
	    (run-hooks 'org-clock-in-hook)))))))


(defun mj/org-clock-select-task (&optional prompt)
  "Select a task that was recently associated with clocking."
  (interactive)
  (let (och chl sel-list rpl (i 0) s)
    ;; Remove successive dups from the clock history to consider
    (mapc (lambda (c) (if (not (equal c (car och))) (push c och)))
	  org-clock-history)
    (setq och (reverse och) chl (length och))
    (if (zerop chl)
	(user-error "No recent clock")
      (save-window-excursion
	(org-switch-to-buffer-other-window
	 (get-buffer-create "*Clock Task Select*"))
	(erase-buffer)
	(when (marker-buffer org-clock-default-task)
	  (insert (org-add-props "Default Task\n" nil 'face 'bold))
	  (setq s (org-clock-insert-selection-line ?d org-clock-default-task))
	  (push s sel-list))
	(when (marker-buffer org-clock-interrupted-task)
	  (insert (org-add-props "The task interrupted by starting the last one\n" nil 'face 'bold))
	  (setq s (org-clock-insert-selection-line ?i org-clock-interrupted-task))
	  (push s sel-list))
	(when (org-clocking-p)
	  (insert (org-add-props "Current Clocking Task\n" nil 'face 'bold))
	  (setq s (org-clock-insert-selection-line ?c org-clock-marker))
	  (push s sel-list))
	;; what I need to make a dedicated lunch clock-in entry:
	;; mimic the stuff in the (when) clauses above.
	;; use ?l as the character for selection.
	;; for the marker, call the function that makes one from an org task's ID: can reuse code from my init.el where I make a marker from the ID for "Organization"
	;; combine all of the above and that ought to do it!
	(progn
	  (insert (org-add-props "Favorite Tasks\n" nil 'face 'bold))
	  (setq s (org-clock-insert-selection-line ?l mj-lunch-marker))
	  (push s sel-list)
	  (setq s (org-clock-insert-selection-line ?b mj-break-marker))
	  (push s sel-list))
	(insert (org-add-props "Recent Tasks\n" nil 'face 'bold))
	(mapc
	 (lambda (m)
	   (when (marker-buffer m)
	     (setq i (1+ i)
		   s (org-clock-insert-selection-line
		      (if (< i 10)
			  (+ i ?0)
			(+ i (- ?A 10))) m))
	     (if (fboundp 'int-to-char) (setf (car s) (int-to-char (car s))))
	     (push s sel-list)))
	 och)
	(run-hooks 'org-clock-before-select-task-hook)
	(goto-char (point-min))
	;; Set min-height relatively to circumvent a possible but in
	;; `fit-window-to-buffer'
	(fit-window-to-buffer nil nil (if (< chl 10) chl (+ 5 chl)))
	(message (or prompt "Select task for clocking:"))
	(setq cursor-type nil rpl (read-char-exclusive))
	(cond
	 ((eq rpl ?q) nil)
	 ((eq rpl ?x) nil)
	 ((assoc rpl sel-list) (cdr (assoc rpl sel-list)))
	 (t (user-error "Invalid task choice %c" rpl)))))))
