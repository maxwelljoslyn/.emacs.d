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
  :init
  (apheleia-global-mode +1))

(use-package magit
  :bind ("C-x g" . magit))


;; GUI tweaks.
(tool-bar-mode -1)
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)

;; Other tweaks.
(setq confirm-kill-emacs #'yes-or-no-p)
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(defalias 'yes-or-no #'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)
;; End a sentence with full stop plus 1 space, not 2 spaces.
(setq sentence-end-double-space nil)
;; Enable narrow-to-region.
(put 'narrow-to-region 'disabled nil)


;; Maximize initial frame.
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Start up in a known location, with known window size.
(find-file "~/.emacs.d/init.el")
(split-window-horizontally)
(find-file "~/Desktop/projects/dnd/dnd/class_tables.py")


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
