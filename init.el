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

(use-package eglot
  :straight (:type built-in)
  :config
  (add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))
  :hook
  (python-mode . eglot))

;; Installation of third-party packages.

(use-package flymake-ruff
  :hook
  ;; (python-mode . flymake-ruff-mode)
  (eglot-managed-mode . flymake-ruff-load))

(use-package apheleia
  :config
  (add-to-list 'apheleia-mode-alist '(python-mode . ruff))
  (apheleia-global-mode +1))

(use-package magit
  :bind ("C-x g" . magit))

(use-package which-key
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-mode))


(defvar mj/file-map
  ;; defvar so it doesn't get re-evaluated; use C-M-x to eval
  (let ((map (make-sparse-keymap)))
    (define-key map "f" #'find-file)
    (define-key map "r" #'rename-file)
    map)
  "Map for working with files.")

(defvar mj/prefix-map
  ;; defvar so it doesn't get re-evaluated
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") #'execute-extended-command)
    (define-key map "1" #'delete-other-windows)
    (define-key map "o" #'save-buffer)
    (define-key map "f" mj/file-map)
    map)
  "Super secret special keymap.")

(use-package evil
  :init
  (setq evil-want-C-i-jump nil)
  :config
  ;; TODO this doesn't work. Think it's spacemacs only.
  (setq-default evil-escape-key-sequence "kl")
  (evil-set-undo-system 'undo-redo)
  (evil-define-key 'normal 'global "m" 'evil-search-forward)
  (evil-define-key 'normal 'global "M" 'evil-search-backward)
  (evil-define-key 'visual 'global "m" 'evil-search-forward)
  (evil-define-key 'visual 'global "M" 'evil-search-backward)
  (evil-define-key 'visual 'magit-mode-map "s" 'magit-stage)
  (evil-define-key 'visual 'global-map "s" 'evil-surround-edit)

  (evil-define-key 'normal 'dired-mode-map "n" 'evil-search-next)
  ;; TODO which of these is better:
  ;; 1
  (define-key evil-normal-state-map (kbd "SPC") mj/prefix-map)
  ;; 2
  ;;(evil-define-key 'normal 'global (kbd "SPC") mj/prefix-map)
  ;; 3 ;; idk if this works
  ;;(evil-set-leader nil (kbd "SPC"))
  (evil-mode 1))

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
	setq enable-recursive-minibuffers t
	;; TODO this isn't working...
	;; So you can, for instance, create a file named "hat" in a
	;; directory that already has a file named "hatter"
	setq ivy-use-selectable-prompt t)
  :config
  (ivy-mode)
  (counsel-mode))





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
 lazy-highlight-buffer t)
(setq
 window-resize-pixelwise t
 frame-resize-pixelwise t)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(defalias 'yes-or-no-p #'y-or-n-p)
(setq dired-deletion-confirmer #'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)
;; End a sentence with full stop plus 1 space, not 2 spaces.
(setq sentence-end-double-space nil)
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



;; Maximize initial frame.
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Start up in a known location.
(find-file "~/.emacs.d/init.el")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(tsdh-dark)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
