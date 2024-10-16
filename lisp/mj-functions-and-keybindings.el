(defun cancel-current-operation ()
  (condition-case nil
      (keyboard-quit)
    (quit nil)))

(defun mj/turn-off-flymake () (flymake-mode -1))

(defun mj/magit-keys ()
  ;; NOTE Evil defaults SOMETIMES unbinding my SPC prefix command in Magit still,
  ;; that's why it's hardcoded here to be unbound (so I can rebind it)
  (unbind-key "SPC" magit-mode-map)
  (evil-make-overriding-map magit-mode-map 'normal)
  (evil-define-key 'normal magit-mode-map (kbd "SPC") mj/prefix-map)
  (evil-define-key 'normal magit-mode-map (kbd "s") 'magit-stage)
  (evil-make-overriding-map magit-mode-map 'visual)
  (evil-define-key 'visual magit-mode-map (kbd "s") 'magit-stage))


(defun delete-visited-file ()
  (interactive)
  (let ((filename (buffer-file-name))
	bufname (buffer-name))
    (when (y-or-n-p "Delete this buffer's file?")
      (delete-file filename)
      (kill-buffer bufname))))

(defun mj/save-all-buffers ()
  (interactive)
  (save-some-buffers t))

(setq mj/buffer-map
      (let ((map (make-sparse-keymap)))
	(define-key map "b" #'switch-to-buffer)
	(define-key map "n" #'next-buffer)
	(define-key map "p" #'previous-buffer)
	map))

(setq mj/file-map
      (let ((map (make-sparse-keymap)))
	(define-key map "f" #'find-file)
	(define-key map "r" #'rename-visited-file)
	(define-key map "d" #'delete-visited-file)
	map))

(setq mj/window-map
      (let ((map (make-sparse-keymap)))
	(define-key map "j" #'buf-move-down)
	(define-key map "k" #'buf-move-up)
	(define-key map "l" #'buf-move-right)
	(define-key map "h" #'buf-move-left)
	map))

(setq mj/text-map
      (let ((map (make-sparse-keymap)))
	(define-key map "r" #'vr/replace)
	map))

(setq mj/prefix-map
      (let ((map (make-sparse-keymap)))
	(define-key map (kbd "SPC") #'execute-extended-command)
	(define-key map "1" #'delete-other-windows)
	(define-key map "o" #'mj/save-all-buffers)
	(define-key map "f" mj/file-map)
	(define-key map "b" mj/buffer-map)
	(define-key map "w" mj/window-map)
	(define-key map "t" mj/text-map)
	map))

(provide 'mj-functions-and-keybindings)
