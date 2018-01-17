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
