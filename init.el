(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; fix the PATH variable
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(if window-system (set-exec-path-from-shell-PATH))


;; load the ensime lisp code...
;; (add-to-list 'load-path "/usr/local/share/ensime/ensime_2.10.0-0.9.8.9/elisp/")
;; (require 'ensime)

;; This step causes the ensime-mode to be started whenever
;; scala-mode is started for a buffer. You may have to customize this step
;; if you're not using the standard scala mode.
;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
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
                      scala-mode2
                      starter-kit
                      starter-kit-lisp
                      starter-kit-bindings
                      starter-kit-ruby)
  "A list of packages to ensure are installed at launch.")

(set-face-attribute 'default nil :font "Source Code Pro-14" )
(set-frame-font "Source Code Pro-14"  nil t)

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(global-linum-mode t)

(load "~/.emacs.d/user.el")
