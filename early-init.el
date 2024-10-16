;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html

;; This is partially to ensure that LSP communication is smooth, because those roundtrips generates a lot of garbage.
;; If you keep having garbage collection problems, double it.
;; If even a very large increase to garbage collector threshold
;; doesn't change responsiveness problems, the issue probably isn't
;; coming from garbage collection.
(setq gc-cons-threshold 25000000) ;; ~25 Mb

(setq read-process-output-max (* 1024 1024)) ;; 1 MB

;; https://github.com/radian-software/straight.el#getting-started
;; "Users of Emacs versions >= 27 will want to add..."
(setq package-enable-at-startup nil)

;; from https://github.com/KaratasFurkan/.emacs.d#settings
;; Straight uses symlinks in the build directory which causes xref-find-definition to ask:
;; "Symbolic link to Git-controlled source file; follow link? (y or n)"
;; every time. To always answer yes, set vc-follow-symlinks.
(setq vc-follow-symlinks t)
;; Default depth of 1 when cloning package repositories.
(setq straight-vc-git-default-clone-depth 1)

