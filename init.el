;; Bootstrap straight.el (replacement for built-in package.el package manager).
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

(add-to-list 'load-path "~/.emacs.d/lisp")

;; Configure packages built into Emacs.
(require 'mj-builtins)

;; Configure my custom functions and keybindings.
(require 'mj-functions-and-keybindings)

;; Install and configure third-party packages.



(use-package eglot-booster
  :after (eglot)
  :straight
  (eglot-booster :type git :host github :repo "jdtsmith/eglot-booster")
  :config
  (eglot-booster-mode))



(use-package buffer-move
  :straight
  (buffer-move :type git :host github :repo "lukhas/buffer-move"))


(use-package explain-pause-mode
  :straight
  (explain-pause-mode :type git :host github :repo "lastquestion/explain-pause-mode")

  :config
  (explain-pause-mode))

(use-package apheleia
  :config
  ;; "ruff check" replaces isort
  ;; "ruff format" replaces black
  (setf (alist-get 'ruff-format apheleia-formatters)
	'("ruff" "format" "--silent"
	  (apheleia-formatters-fill-column "--line-length")
	  "--stdin-filename" filepath "-"))
  (setf (alist-get 'ruff-check apheleia-formatters)
	'("ruff" "check" "--select" "I" "--fix" "--silent"
	  (apheleia-formatters-fill-column "--line-length")
	  "--stdin-filename" filepath "-"))
  (setf (alist-get 'python-mode apheleia-mode-alist)
	'(ruff-check ruff-format))
  ;; html
  ;; needs jinja-template installed:
  ;; npm install prettier prettier-plugin-jinja-template
  ;; also needs a .prettierrc in the given project's root
  (setf (alist-get 'mj-prettier apheleia-formatters)
	'("prettier"  "--stdin-filepath" filepath "--plugin=prettier-plugin-jinja-template" "--parser=jinja-template"))
  (setf (alist-get 'html-mode apheleia-mode-alist)
	'(mj-prettier))
  ;; js
  (setf (alist-get 'mj-js apheleia-formatters)
	'("prettier" "--stdin-filepath" filepath "--parser=babel-flow" "--use-tabs=false"))
  (setf (alist-get 'js-mode apheleia-mode-alist)
	'(mj-js))

  (apheleia-global-mode +1))

(use-package casual-dired
  :straight
  (casual-dired-mode :type git :host github :repo "kickingvegas/casual-dired")
  :bind
  (:map dired-mode-map ("C-o" . 'casual-dired-tmenu))
  (:map dired-mode-map ("s" . 'casual-dired-sort-by-tmenu))
  (:map dired-mode-map ("E" . 'wdired-change-to-wdired-mode)))


(use-package which-key
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-mode))

(use-package markdown-mode)


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
	ivy-use-selectable-prompt t
	ivy-initial-inputs-alist nil)
  :config
  (ivy-mode)
  (counsel-mode))

(use-package evil-escape
  :straight
  (evil-escape :type git :host github :repo "smile13241324/evil-escape")
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
 '(apheleia-formatters-respect-fill-column nil)
 '(apheleia-formatters-respect-indent-level t)
 '(apheleia-global-mode t)
 '(custom-enabled-themes '(tsdh-dark))
 '(dired-use-ls-dired 'unspecified)
 '(eglot-booster-mode t)
 '(eglot-events-buffer-size 0)
 '(lazy-highlight-buffer t)
 '(lazy-highlight-cleanup nil)
 '(lazy-highlight-initial-delay 0))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight ((t (:background "chocolate3")))))
