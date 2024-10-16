
;; These are to make evil collection work correctly. They have to be loaded before evil or anything that loads evil.
;; But if I put them in the :init keyword of use-package, they still don't get loaded in time. So, I put them outside.
(setq evil-want-keybinding nil
      evil-want-integration t
      evil-want-C-i-jump nil)

(require 'mj-functions-and-keybindings)

(use-package evil
  :after (magit)
  :config
  (evil-mode 1)
  )

(use-package evil-collection
  :after (evil)
  :config
  (evil-collection-init '(magit dired xref)))

(use-package evil-surround
  :after (evil)
  :config
  (global-evil-surround-mode 1)
  ;; double asterisk for markdown "b"old
  (setq-default evil-surround-pairs-alist
		(push '(?b . ("**" . "**")) evil-surround-pairs-alist))
  ;; double bracket for wiki "l"ink syntax
  (setq-default evil-surround-pairs-alist
		(push '(?l . ("[[" . "]]")) evil-surround-pairs-alist))
  ;; "a"rt-title HTML class
  (setq-default evil-surround-pairs-alist
		(push '(?a . ("<span class=\"art-title\">" . "</span>")) evil-surround-pairs-alist))
  ;; double "u"nderscore dunder for python dunder identifiers
  (add-hook 'python-mode-hook (lambda ()
				(push '(?u . ("__" . "__")) evil-surround-pairs-alist)))
  ;; triple "q"uotes for python multiline strings
  (add-hook 'python-mode-hook (lambda ()
				(push '(?t . ("\"\"\"" . "\"\"\"")) evil-surround-pairs-alist))))

(use-package evil-escape
  :after (evil)
  :straight
  (evil-escape :type git :host github :repo "smile13241324/evil-escape")
  :init
  (evil-escape-mode)
  (setq-default evil-escape-key-sequence "jl")
  (setq evil-escape-case-insensitive-key-sequence t))

(defun enable-evil-and-enter-normal-state ()
  "Enable Evil mode and enter normal state when switching to a prog-mode derived buffer."
  (when (derived-mode-p 'prog-mode)
    (evil-mode 1)
    (evil-normal-state)))

(add-hook 'find-file-hook 'enable-evil-and-enter-normal-state)

(defun mj/vr-search-forward (beg end)
  "Start a forward visual regexp search, extending the selection."
  (interactive (if (evil-visual-state-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (if (and beg end)
      (let ((mark (set-marker (make-marker) (mark)))
            (point (set-marker (make-marker) (point))))
        (vr/isearch-forward)
        (evil-visual-make-selection mark (point) 'inclusive))
    (vr/isearch-forward)))

(defun mj/vr-search-backward (beg end)
  "Start a backward visual regexp search, extending the selection."
  (interactive (if (evil-visual-state-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (if (and beg end)
      (let ((mark (set-marker (make-marker) (mark)))
            (point (set-marker (make-marker) (point))))
        (vr/isearch-backward)
        (evil-visual-make-selection mark (point) 'inclusive))
    (vr/isearch-backward)))

(provide 'mj-evil)
