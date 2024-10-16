(use-package python
  :straight (:type built-in)
  :custom
  (python-shell-interpreter "ipython")
  (python-shell-interpreter-args "-i --simple-prompt")
  (python-indent-guess-indent-offset-verbose nil))

(use-package transient :straight (:type built-in))


;; (use-package eglot
;;   :straight (:type built-in)
;;   :config
;;   (fset #'jsonrpc--log-event #'ignore)
;;   (setq eglot-events-buffer-size 0)
;;   (add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))
;;   ;; this should also remove flymake from eglot; doing both just in case
;;   (setq eglot-stay-out-of '(flymake))
;;   :hook
;;   (prog-mode . eglot-ensure)
;;   ;; flymake HUGELY bogs down eglot, pausing Emacs for seconds at a time
;;   (eglot--managed-mode-hook . mj/turn-off-flymake))


;; (use-package eglot-booster
;;   :after (eglot)
;;   :straight
;;   (eglot-booster :type git :host github :repo "jdtsmith/eglot-booster")
;;   :config
;;   (eglot-booster-mode))

(use-package bug-reference-mode
  :straight (:type built-in)
  :config
  :hook
  (prog-mode . bug-reference-prog-mode))


(provide 'mj-builtins)

;; (straight-pull-recipe-repositories 
;;  '(org-elpa melpa gnu-elpa-mirror nongnu-elpa el-get emacsmirror-mirror))
