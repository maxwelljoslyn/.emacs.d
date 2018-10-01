(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)

(defvar at-home
  (file-directory-p "/Users/maxwelljoslyn/Desktop/projects/"))

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("MELPA Stable" . "https://stable.melpa.org/packages/")
	("melpa" . "https://melpa.org/packages/")
	("gnu" . "https://elpa.gnu.org/packages/")
	("org" . "https://orgmode.org/elpa/")))
(package-initialize)

;; to run certifi, you will need to pip install -m certifi beforehand
;; gnutls will also need to be installed beforehand; brew and most other package managers have it
(let ((trustfile
       (replace-regexp-in-string
        "\\\\" "/"
        (replace-regexp-in-string
         "\n" ""
         (shell-command-to-string "python3 -m certifi")))))
  (setq tls-program
        (list
         (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                 (if (eq window-system 'w32) ".exe" "") trustfile)))
  (setq gnutls-verify-error t)
  (setq gnutls-trustfiles (list trustfile)))

;; Save the running clock and all clock history when exiting Emacs, load it on startup
(org-clock-persistence-insinuate)
(setq org-clock-persist 'history)
;; Show lot of clocking history
(setq org-clock-history-length 23)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; when t, continue clocking into another task upon clock-out of one task
(setq org-clock-continuously nil)
;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
(setq org-clock-out-when-done t)

;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

(defvar mj/organization-task-id "A3ACA243-6D4A-4CC3-8238-B8FD2FDB6C56")
(setq org-clock-default-task (org-id-find mj/organization-task-id 'marker))

(defvar mj/lunch-task-id "2FCFBF87-3B65-4C66-8CF4-2A94E5D02920")
(setq mj-lunch-marker (org-id-find mj/lunch-task-id 'marker))

(defvar mj/break-task-id "B926C6A0-A876-4815-BB47-56366D507A21")
(setq mj-break-marker (org-id-find mj/break-task-id 'marker))

(defun mj/clock-in-default-task ()
  (interactive)
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun mj/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find mj/organization-task-id 'marker)
    (org-clock-in '(16))))

(setq mj/keep-clock-running nil)

(defun mj/punch-in ()
  "Start \"continuous clocking\" and clock into Organization, setting it as the default task.
Derived from Norang setup."
  (interactive)
  (mj/clock-in-organization-task-as-default)
  (setq org-clock-continuously t)
  (setq mj/keep-clock-running t))

(defun mj/punch-out ()
  (interactive)
  (setq mj/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (setq org-clock-continuously nil))

(defun mj/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))


(defun mj/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when mj/keep-clock-running
            (mj/clock-in-default-task)))))))

(defun mj/clock-out-maybe ()
  (when (and mj/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (mj/clock-in-parent-task)))

(add-hook 'org-clock-out-hook 'mj/clock-out-maybe 'append)
;; Remove empty LOGBOOK drawers on clock out
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK" (point))))

(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)


;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

(load "~/.emacs.d/lisp/mj-clock.el")
(defun mj/clock-in-with-prefix ()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'mj/org-clock-in)))
(global-set-key (kbd "C-9") 'mj/clock-in-with-prefix)


(defun mj/org-clocktable-indent-string (level)
  (if (= level 1)
      "╰"
    (let ((str "╰"))
      (while (> level 2)
        (setq level (1- level)
              str (concat str "─")))
      (concat str "─> "))))

(advice-add 'org-clocktable-indent-string :override #'mj/org-clocktable-indent-string)

;; bootstrap use-package
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))


;; (add-to-list 'auto-mode-alist '("\\.tex\\'" . latex-mode))
(add-to-list 'auto-mode-alist '("\\.ledger\\'" . ledger-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

(setq use-package-always-ensure t)

(setq-default abbrev-mode t)

(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

(use-package which-key
	     :config
	     (which-key-mode))

(defun mj/insert-filename (filename &optional args)
    "Insert path to file FILENAME into buffer after point.
  
  Prefixed with \\[universal-argument], expand the file name to
  its fully canonicalized path.  See `expand-file-name'.
  
  Prefixed with \\[negative-argument], use relative path to file
  name from current directory, `default-directory'.  See
  `file-relative-name'.
  
  The default with no prefix is to insert the file name exactly as
  it appears in the minibuffer prompt."
    (interactive "*fInsert file name: \nP")
    (cond ((eq '- args)
           (insert (file-relative-name filename)))
          ((not (null args))
           (insert (expand-file-name filename)))
          (t
           (insert filename))))

(use-package ess)

(defun mj/org-save-and-commit ()
  (interactive)
  (progn
    (save-buffer)
    (shell-command (expand-file-name "~/Desktop/commit_todo.sh"))))

(setq mj/todo-org-local-buffer-mode-map (make-sparse-keymap))

(define-minor-mode mj/todo-org-local-buffer-mode
    "Minor mode to simulate buffer local keybindings."
    :init-value nil)
(define-key mj/todo-org-local-buffer-mode-map (kbd "C-x C-s") 'mj/org-save-and-commit)

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(defun find-org-file ()
  (interactive)
  (find-file "~/Desktop/todo.org"))

;; Exclude DONE state tasks from refile targets
;; from doc.norang.ca
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) 'org-done-keywords)))

(global-set-key (kbd "M-q") 'toggle-truncate-lines)

(use-package org
  :config
  (global-set-key (kbd "<f8>") 'org-agenda)
  (global-set-key (kbd "C-c b") 'find-org-file)
  (global-set-key (kbd "C-8") 'org-capture)

  (define-key org-mode-map (kbd "C-M-RET") 'org-insert-subheading)
  (define-key org-mode-map (kbd "C-'") nil)
  (define-key org-mode-map (kbd "C-c t") 'org-todo)
  (define-key org-mode-map (kbd "C-c C-.") 'org-time-stamp-inactive)

  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
  (setq org-log-done 'time)
  (setq org-default-notes-file "~/Desktop/todo.org")
  (setq org-log-into-drawer t)
  ;; (setq org-log-into-drawer t)


  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)" "CANCELLED(c)")
	  (sequence "WAIT(w!)" "HOLD(h)" "|" "DONE(d)" "CANCELLED(c)")))

  (setq org-todo-keyword-faces
	(quote (("TODO" :foreground "red" :weight bold)
		("NEXT" :foreground "deep sky blue" :weight bold)
		("DONE" :foreground "forest green" :weight bold)
		("WAIT" :foreground "Darkorange1" :weight bold)
		("HOLD" :foreground "Pink2" :weight bold)
		("CANCELLED" :foreground "Olivedrab4" :weight bold))))

  (setq org-agenda-custom-commands
	'(("d" "daily driver agenda command"
	   ((agenda "" ((org-agenda-span 1)))
	    (todo "WAIT")
	    (todo "NEXT")))))

  (setq org-capture-templates
	(quote (("t" "todo" entry (file "~/Desktop/todo.org") "* TODO %? %^{Effort}p" :clock-in t :clock-resume t)
		("n" "next" entry (file "~/Desktop/todo.org") "* NEXT %?" :clock-in t :clock-resume t)
		("w" "wait" entry (file "~/Desktop/todo.org") "* WAIT %?" :clock-in t :clock-resume t)
		("h" "hold" entry (file "~/Desktop/todo.org") "* HOLD %?\n SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+1w\"))" :clock-in t :clock-resume t)
		("v" "vocabulary item" entry (file+headline "~/Desktop/todo.org" "Chinese vocab") "* NEXT Add to Anki: %^{Word/phrase} :chinese:%U")
		("p" "plain" entry (file "~/Desktop/todo.org") "* %?" :clock-in t :clock-resume t)
		("e" "beeminder" entry (file+headline "~/Desktop/todo.org" "things to Beemind") "* HOLD beemind %?\n SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+1w\"))")
		("b" "buy" entry (file+headline "~/Desktop/todo.org" "things to buy") "* NEXT %?")
		("m" "Media prefix")
		("mw" "watch" entry (file+headline "~/Desktop/todo.org" "media") "* WATCH %?")
		("mp" "play" entry (file+headline "~/Desktop/todo.org" "media") "* PLAY %?")
		("ml" "listen" entry (file+headline "~/Desktop/todo.org" "media") "* LISTEN %?")
		("mr" "read" entry (file+headline "~/Desktop/todo.org" "media") "* READ %?")
		("ma" "already watched/read/etc." entry (file+olp "~/Desktop/todo.org" "media" "already done") "* DONE %?")
                ("f" "focused writing (e.g. character/location study)" entry (file+olp "~/Desktop/todo.org" "focused writing") "* %?"))))


  ;; targets include any file which goes into the agenda, up to 3 levels deep
  (setq org-refile-targets (quote ((org-agenda-files :maxlevel . 3))))
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes (quote confirm))
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)
  (setq org-refile-target-verify-function 'bh/verify-refile-target)
  (setq org-export-initial-scope 'subtree)
  (setq org-catch-invisible-edits 'show-and-error)
  (setq org-agenda-files '("~/Desktop/todo.org"))
  (setq org-special-ctrl-a/e t)
  (setq org-export-with-smart-quotes nil
	org-export-with-emphasize t
	org-export-with-sub-superscripts '{}
	org-export-with-footnotes t
	org-export-with-toc t
	org-export-headline-levels 2
	org-use-fast-tag-selection t
	org-agenda-skip-scheduled-if-done t
    org-agenda-skip-deadline-if-done t))

;; this has to be done AFTER org-agenda has loaded so that the consistency-checks variable exists
;; putting it outside the org-mode use-package form seems to ensure this, and prevent errors on load
(defvar org-agenda-clock-consistency-checks
  '(:max-duration "10:00" :min-duration 0 :max-gap "0:00" :gap-ok-around
               ("4:00")
               :default-face
               ((:background "DarkRed")
                (:foreground "white"))
               :overlap-face nil :gap-face nil :no-end-time-face nil :long-face nil :short-face nil))


(use-package swoop)

(use-package helm-swoop
  :config
  (setq helm-swoop-pre-input-function '(lambda () nil)))

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

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(setq seoul256-background 235)
(use-package seoul256-theme)



(use-package avy)
(use-package ace-window
  :bind
  ("C-6" . ace-window)
  :config
  (setq aw-keys '(?d ?f ?s ?e ?g ?h)))

(use-package dired-subtree
  :config
  (define-key dired-mode-map (kbd "i") 'dired-subtree-insert)
  (define-key dired-mode-map (kbd ";") 'dired-subtree-remove))

(use-package dired-filter)

(use-package iedit
  :commands
  (iedit-mode)
  :bind
  ("C-;" . iedit-mode))

(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "C-2") 'split-window-below)
(global-set-key (kbd "C-3") 'split-window-horizontally)
(global-set-key (kbd "C-4") 'ctl-x-4-prefix)
(global-set-key (kbd "C-5") 'ctl-x-5-prefix)

(use-package company
  :config
  (setq company-global-modes '(not text-mode))
  (setq company-idle-delay 0.4)
  (add-hook 'after-init-hook global-company-mode))

(use-package web-mode
  :mode ("\\.html$" . web-mode))

;; brings in my environment and path variables from bash so I call executables such as pdflatex
(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

;; allows jumping to Chinese characters with avy based on pinyin
;; (use-package ace-pinyin
;;   :config
;;   (ace-pinyin-mode)
;;   (setq ace-pinyin-simplified-chinese-only-p nil))

(use-package slime
  :config
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  (setq slime-contribs '(slime-fancy))
  )


;; I plain have not used these at all
;; (use-package corral
;;   :config
;;   (global-set-key (kbd "M-9") 'corral-parentheses-backward)
;;   (global-set-key (kbd "M-0") 'corral-parentheses-forward)
;;   (global-set-key (kbd "M-[") 'corral-brackets-backward)
;;   (global-set-key (kbd "M-]") 'corral-brackets-forward))

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (global-set-key (kbd "C-/") nil)
  (global-set-key (kbd "C-?") nil)
  (setq undo-tree-auto-save-history t)
  (let ((undo-tree-dir (expand-file-name "~/.emacs.d/undo-tree/")))
    (setq undo-tree-history-directory-alist (list (cons "." undo-tree-dir)))))

(use-package markdown-mode
  :commands (markdown-mode)
  :mode (("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode)))

(use-package magit
  :bind
  (("C-x g" . magit-status)))

;; Can't get helm to work thru use-package, so I'll just install it the ordinary way
(unless (package-installed-p 'helm)
  (package-refresh-contents)
  (package-install 'helm))
(require 'helm)
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-o") 'helm-mini)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-c h m") 'helm-all-mark-rings)
(global-set-key (kbd "C-c h w") 'helm-man-woman)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-o") 'helm-swoop)
(global-set-key (kbd "C-c g") 'helm-do-grep-ag)
;; eval an elisp sexp with live results -- improvement on default M-:
(global-set-key (kbd "M-:") 'helm-eval-expression-with-eldoc)
(global-set-key (kbd "C-h a") 'helm-apropos)
(setq helm-grep-ag-command "rg --color=always --colors 'match:fg:black' --colors 'match:bg:yellow' --smart-case --no-heading --line-number %s %s %s")
(setq helm-grep-ag-pipe-cmd-switches '("--colors 'match:fg:black'" "--colors 'match:bg:yellow'"))
(setq helm-M-x-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-mode-fuzzy-match t
      helm-apropos-fuzzy-match t
      helm-completion-in-region-fuzzy-match t)
(setq dirtrack-mode 1)

(setq delete-by-moving-to-trash t)

(use-package haskell-mode)

(setq doc-view-continuous t)


(use-package yasnippet)


(use-package expand-region
  :bind
  (("C-\\" . er/expand-region)))


(use-package visual-regexp-steroids
  :ensure visual-regexp
  :bind
  (("C-c r" . vr/replace)
   ("C-c M-r" . vr/query-replace)
   ("C-M-r" . vr/isearch-backward)
   ("C-M-s" . vr/isearch-forward)))

;; (use-package flycheck
;; (add-hook 'after-init-hook #'global-flycheck-mode)

;;mode-specific-map is the global keymap for the prefix key C-c
;;alternatively, use (define-prefix-command 'my-lmao-map), then (global-set-key (kbd "M-o") my-lmao-map)
;; then you can use (define-key my-lmao-map (kbd "x") 'do-shit)

(define-prefix-command 'mj-mc-map)
(global-set-key (kbd "C-c m") 'mj-mc-map)
(use-package multiple-cursors)
(define-key mj-mc-map (kbd "e") 'mc/edit-lines)
(define-key mj-mc-map (kbd "r") 'mc/mark-all-in-region-regexp)
(define-key mj-mc-map (kbd "n") 'mc/mark-next-like-this)
(define-key mj-mc-map (kbd "d") 'mc/mark-all-dwim)

(use-package define-word)


(use-package paredit
  :ensure t
  :config
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'racket-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'lisp-mode-hook 'paredit-mode))




(when at-home
    (defun journal (arg)
      "Find today's journal file. With prefix ARG, open yesterday's file."
      (interactive "P")
      (let ((journal-name
             (if (equal arg nil)
                 (format-time-string "%Y_%m_%d")
               (format-time-string "%Y_%m_%d" (time-subtract (current-time) (seconds-to-time (* 24 3600)))))))
        (find-file (expand-file-name (concat "~/Desktop/projects/Journal/Journal_" journal-name ".org")))))
  (global-set-key (kbd "C-c j") 'journal))

;; make backups go into their own folder
;; I think it works but idk
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))

;; miscellaneous bindings
;; unbind C-x f since I hardly ever use it and it is similar to C-x C-f
(global-set-key (kbd "C-x f") nil)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
; allow response to prompts to be y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)
; make a sentence be full stop plus 1 space, instead of 2 spaces
(setq sentence-end-double-space nil)
; enable narrow-to-region
(put 'narrow-to-region 'disabled nil)
(global-set-key (kbd "M-/") 'hippie-expand)
;; turn off these STUPID shortcuts which minimizes the window! and it's right next to C-x!
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "C-x C-z") nil)
;; start initial frame maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(global-set-key (kbd "M-SPC") 'delete-horizontal-space)


(global-set-key (kbd "C-x C-c") nil)

(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename t) ; t at end means "move to trash instead of deleting""
          (message "Deleted file %s" filename)
          (kill-buffer))))))


;; (global-set-key (kbd "M-j")
;; 		(lambda ()
;; 		  (interactive)
;; 		  (join-line -1)))
		

(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir)))))

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  ;; http://whattheemacsd.com/file-defuns.el-01.html
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

(defun mj/latex-macron ()
    (interactive)
    (progn
      (insert (kbd "\\={"))
      (if (string= (string (char-after)) "i")
	  (insert (kbd "\\"))
	)
      (forward-char)
      (insert (kbd "}"))
      )
    )

(defun mj/latex-caron ()
    (interactive)
    (progn
      (insert (kbd "\\v{"))
       (if (string= (string (char-after)) "i")
	  (insert (kbd "\\"))
	)
      (forward-char)
      (insert (kbd "}"))
      )
    )

(defun mj/latex-acute ()
    (interactive)
    (progn
      (insert (kbd "\\\'{"))
       (if (string= (string (char-after)) "i")
	  (insert (kbd "\\"))
	)
      (forward-char)
      (insert (kbd "}"))
      )
    )

(defun mj/latex-keys ()
  (progn
    (local-set-key (kbd "C-c =") 'mj/latex-macron)
    (local-set-key (kbd "C-c v") 'mj/latex-caron)
    (local-set-key (kbd "C-c 2") 'mj/latex-acute)
    )
  )

(add-hook 'LaTeX-mode-hook 'mj/latex-keys)
(add-hook 'latex-mode-hook 'mj/latex-keys)

(defun mj/haskell-keys ()
  (progn
    (local-set-key (kbd "C-c l") 'haskell-process-load-file))
  )

(setq ledger-reports
   (quote
    (("bal, real" "%(binary) -f %(ledger-file) bal --real")
     ("bal" "%(binary) -f %(ledger-file) bal")
     ("monthly expenses" "%(binary) -f %(ledger-file) reg -AMn --empty ^Expenses --real")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)"))))
(setq ledger-highlight-xact-under-point nil)

;; set default font size to 16
(set-face-attribute 'default nil :font "Menlo:pixelsize=16:weight=normal:slant=normal:width=normal:spacing=100:scalable=true" )

;; never use tabs
(setq-default indent-tabs-mode nil)
(setq tab-width 4)



;; find favorites unless they're already visited
;; this stops Emacs from switching over to that file if I'm just evaling my whole init.el while tweaking it
(let ((favorite-files '("~/Desktop/todo.org" "~/.emacs.d/init.el")))
  (when at-home
      (add-to-list 'favorite-files "/Users/maxwelljoslyn/Desktop/projects/finance.ledger"))
  (dolist (element favorite-files)
    (unless (get-file-buffer element)
      (find-file element))))


(defun mj/jdate ()
  "Returns date as yyyy_mm_dd."
  (interactive)
  (format-time-string "%Y_%m_%d"))

(defun mj/insert-jdate ()
  (interactive)
  (insert (mj/jdate)))

(defun mj/fdate ()
  "Returns current date in the format yyyy/mm/dd.
This is the format Ledger requires."
  (interactive)
  (format-time-string "%Y/%m/%d"))

(defun mj/insert-fdate ()
  (interactive)
  (insert (mj/fdate)))

(setq python-shell-interpreter "python3")
(show-paren-mode 1)
(setq org-babel-python-command "python3")
(setq org-babel-load-languages (quote ((emacs-lisp . t) (python . t))))
(setq magit-dispatch-arguments nil)

;; (add-to-list 'load-path "~/.emacs.d/evil")
;; (require 'evil)

(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))
