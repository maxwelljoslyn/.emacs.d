;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html

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

