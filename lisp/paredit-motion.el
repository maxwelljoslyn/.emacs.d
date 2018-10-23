;;; -*- lexical-binding:t -*-
(require 'paredit)

;; todo
;; make it play nice with show-paren-mode, such as by letting its colors get overwritten by those of show-paren mode
;; get rid of development conveniences like (goto-char (point-max)) and (sit-for 1)

(defun mj/show-paredit-forward-endpoint ()
  (interactive)
  (save-excursion
    (paredit-forward)
    (setq end (point))
    (backward-char)
    (let* ((beg (point))
           (marker (make-overlay beg  end)))
      (overlay-put marker 'face  '(:foreground "red"))
      (overlay-put marker 'display "F")
      (goto-char (point-max))
      (sit-for 1)
      (remove-overlays (point-min) (point-max)))))


(defun mj/show-multiple-motion-endpoints ()
  (interactive)
  (save-excursion ;; return to the original point after running the code (goto-char (point-max)) (sit-for 1) etc.
    (dolist (a-fun '(paredit-forward paredit-backward paredit-backward-up paredit-forward-up))
      (save-excursion ;; run each command from the original starting point
        (funcall a-fun)
        (setq end (-  (point) 1))
        (let* ((beg   (point))
               (marker (make-overlay beg  end)))
          (overlay-put marker 'face  '(:foreground "red"))
          (overlay-put marker 'display "M"))))
    (goto-char (point-max))
    (sit-for 1)
    (remove-overlays (point-min) (point-max))))


(progn
  (princ (+ 1 2))
  (message (concast "blasto" (concat "bing" "bong") "ghkkg")))

