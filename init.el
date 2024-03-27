;; Bootstrap straight.el (replacement for builtin package.el package manager).
(setq straight-use-package-by-default t)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package (which will make use of straight under the hood).
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Configuring builtin packages.

(use-package python
  :straight (:type built-in)
  :custom
  (python-shell-interpreter "ipython")
  (python-shell-interpreter-args "-i --simple-prompt")
  (python-indent-guess-indent-offset-verbose nil))

(defun mj/turn-off-flymake () (flymake-mode -1))

(use-package eglot
  :straight (:type built-in)
  :config
  (add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))
  ;; this should also remove flymake from eglot; doing both just in case
  (setq eglot-stay-out-of '(flymake))
  :hook
  (prog-mode . eglot-ensure)
  ;; flymake HUGELY bogs down eglot, pausing Emacs for seconds at a time
  (eglot--managed-mode-hook . mj/turn-off-flymake))

;; Installation of third-party packages.

(use-package flymake-ruff
  :hook
  ;; (python-mode . flymake-ruff-mode)
  (eglot-managed-mode . flymake-ruff-load)
  )

(use-package apheleia
  :config
  (add-to-list 'apheleia-mode-alist '(python-mode . ruff))
  (apheleia-global-mode +1))

(use-package which-key
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-mode))

(use-package markdown-mode)

(setq mj/buffer-map
      (let ((map (make-sparse-keymap)))
	(define-key map "b" #'switch-to-buffer)
	(define-key map "n" #'next-buffer)
	(define-key map "p" #'previous-buffer)
	map))

(defun delete-visited-file ()
  (interactive)
  (delete-file (buffer-file-name)))

(setq mj/file-map
      (let ((map (make-sparse-keymap)))
	(define-key map "f" #'find-file)
	(define-key map "r" #'rename-visited-file)
	(define-key map "d" #'rename-visited-file)
	map))

(setq mj/prefix-map
      (let ((map (make-sparse-keymap)))
	(define-key map (kbd "SPC") #'execute-extended-command)
	(define-key map "1" #'delete-other-windows)
	(define-key map "o" #'save-buffer)
	(define-key map "f" mj/file-map)
	(define-key map "b" mj/buffer-map)
	map))

(defun mj/magit-keys ()
  ;; NOTE Evil defaults SOMETIMES unbinding my SPC key in Magit still,
  ;; that's why it's hardcoded here.
  (unbind-key "SPC" magit-mode-map)
  ;; TODO these don't freaking work
  (define-key evil-normal-state-map (kbd "s") 'magit-stage)
  (define-key evil-visual-state-map (kbd "s") 'magit-stage))

(use-package magit
  :bind ("C-x g" . magit)
  :config
  ;; TODO This is conflicting, I think, with (evil-define-key 'visual 'global-map "s")
  (add-hook 'magit-mode-hook 'evil-normal-state 'mj/magit-keys))


(use-package evil
  :after (magit)
  :init
  (setq evil-want-C-i-jump nil
	;; want-keybinding and want-integration are to make evil-collection work
	evil-want-keybinding nil
	evil-want-integration t)
  :config
  (evil-mode 1)
  )

(use-package evil-collection
  :after (evil)
  :config
  (evil-collection-init '(magit dired)))

(use-package evil-surround
  :after (evil)
  :config
  (global-evil-surround-mode 1))

(use-package helpful
  :init
  ;; Built-in `describe-function' includes both functions and macros.
  ;; `helpful-function' is functions only; `helpful-callable' is a
  ;; drop-in replacement.
  (global-set-key (kbd "C-h f") #'helpful-callable)

  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command)

  ;; Look up the current symbol at point. C-c C-d is a common keybinding
  ;; for this in lisp modes.
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)

  ;; Look up *F*unctions (excludes macros).
  ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
  ;; already links to the manual, if a function is referenced there.
  (global-set-key (kbd "C-h F") #'helpful-function))


(use-package counsel)

(use-package ivy
  :after (counsel)
  :init
  (setq ivy-use-virtual-buffers t
	enable-recursive-minibuffers t
	;; TODO this isn't working...
	;; So you can, for instance, create a file named "hat" in a
	;; directory that already has a file named "hatter"
	ivy-use-selectable-prompt t)
  :config
  (ivy-mode)
  (counsel-mode))

(use-package evil-escape
  :straight (evil-escape :type git :host github :repo "smile13241324/evil-escape")
  :init
  (evil-escape-mode)
  (setq-default evil-escape-key-sequence "jl")
  (setq evil-escape-case-insensitive-key-sequence t))

(use-package company
  :ensure t
  :commands (global-company-mode)
  :init
  (global-company-mode)
  :custom
  (company-tooltip-align-annotations 't)
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.1))


;; GUI tweaks.
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)

;; Other tweaks.

(setq-default
 isearch-allow-scroll t
 lazy-highlight-cleanup nil
 lazy-highlight-max-at-a-time nil
 lazy-highlight-initial-delay 0
 lazy-highlight-buffer t
 window-resize-pixelwise t
 frame-resize-pixelwise t
 dired-deletion-confirmer #'y-or-n-p
 ;; don't ask before visiting nonexistent files/buffers
 confirm-nonexistent-file-or-buffer nil
 delete-by-moving-to-trash t
 ;; End a sentence with full stop plus 1 space, not 2 spaces.
 sentence-end-double-space nil)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(defalias 'yes-or-no-p #'y-or-n-p)
;; Enable narrow-to-region.
(put 'narrow-to-region 'disabled nil)
(global-display-line-numbers-mode 1)
;; Disable the font picker.
(global-set-key (kbd "s-t") nil)
(global-set-key (kbd "C-x C-b") #'ibuffer-list-buffers)


;; Create backups in a particular place.
(setq backup-directory-alist `(("." . "~/.saves")))
;; "There are a number of arcane details associated with how Emacs
;; might create your backup files. Should it rename the original and
;; write out the edited buffer? What if the original is linked? In
;; general, the safest but slowest bet is to always make backups by
;; copying."
(setq backup-by-copying t)



(set-frame-font "Source Code Pro 16" nil t)

;; Maximize initial frame.
(add-to-list 'initial-frame-alist '(fullscreen . maximized))


(evil-set-undo-system 'undo-redo)
(evil-define-key 'normal 'global "m" 'evil-search-forward)
(evil-define-key 'normal 'global "M" 'evil-search-backward)
(evil-define-key 'visual 'global "m" 'evil-search-forward)
(evil-define-key 'visual 'global "M" 'evil-search-backward)
(evil-define-key 'visual 'global-map "s" 'evil-surround-edit)
(evil-define-key 'normal 'dired-mode-map "n" 'evil-search-next)
;; Bind the space key everywhere, without binding it in minibuffer or insert mode.
;; NOTE This SPC binding must be set here inside the Evil config, or
;; else Evil defaults will take precedence over it somehow.
;; NOTE Rejected:
;; (define-key evil-normal-state-map (kbd "SPC") mj/prefix-map)
;; (evil-define-key 'normal 'global (kbd "SPC") mj/prefix-map)
;; (evil-set-leader 'normal  (kbd "SPC"))
;; (bind-key* (kbd "SPC") 'mj/prefix-map (not (or (minibufferp) (evil-insert-state-p))))
;; NOTE Rejected:
;; ;; (evil-set-initial-state 'magit-mode 'normal)
;; NOTE Do NOT use (evil-define-key 'visual 'magit-mode-map "s" 'magit-stage)
;; or else the S key will get covered in all visual modes, even outside magit, for some goddamn reason.
(evil-define-key 'normal 'global (kbd "SPC") mj/prefix-map)



;; Start up in a known location.
(find-file "~/.emacs.d/init.el")
(magit)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(tsdh-dark))
 '(lazy-highlight-buffer t)
 '(lazy-highlight-cleanup nil)
 '(lazy-highlight-initial-delay 0))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight ((t (:background "chocolate3")))))
