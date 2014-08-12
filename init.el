(require 'package)

;; Fix path for Emacs gui
;; Don't need to set path for the Windows GUI client
(when (and window-system (eq 'darwin system-type))
  (defun set-exec-path-from-shell-PATH ()
    (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
      (setenv "PATH" path-from-shell)
      (setq exec-path (split-string path-from-shell path-separator))))
  (set-exec-path-from-shell-PATH))

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(ac-nrepl
                      auto-complete
                      cider
                      clojure-mode
                      gist
                      hackernews
                      haskell-mode
                      leuven-theme
                      magit
                      markdown-mode
                      org
                      projectile
                      starter-kit
                      starter-kit-lisp
                      starter-kit-bindings
                      starter-kit-ruby)
  "A list of packages to ensure are installed at launch.")

;; Ensure the above are installed
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(load "~/.emacs.d/user.el")
