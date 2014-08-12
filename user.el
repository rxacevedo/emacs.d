;; This is where your customizations should live

;; Uncomment the lines below by removing semicolons and play with the
;; values in order to set the width (in characters wide) and height
;; (in lines high) Emacs will have whenever you start it

;; (setq initial-frame-alist '((top . 0) (left . 0) (width . 20) (height . 20)))

;; Place downloaded elisp files in this directory. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;; 
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(add-to-list 'load-path "~/.emacs.d/vendor")

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; Themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")

;; Uncomment this to increase font size
;; (set-face-attribute 'default nil :height 140)
;; (load-theme 'tomorrow-night-bright t)
(load-theme 'leuven t)

;; fontify the whole line for headings (with a background color)
(setq org-fontify-whole-heading-line t)

;; Org-mode

;; Set to the location of your Org files on your local system
(setq org-directory "~/org")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/org/flagged.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files (list "~/org/work.org"
                             "~/org/education.org"
                             "~/org/home.org"
                             "~/org/reading.org"
                             "~/org/print_manager.org"))

;; Make top-level headings normal size
;; (set-face-attribute 'org-level-1 nil :height 150)

;; Flyspell often slows down editing so it's turned off
(remove-hook 'text-mode-hook 'turn-on-flyspell)

;; Projectile
(projectile-global-mode)

;; Clojure
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(setq cider-repl-history-file "~/.emacs.d/nrepl-history")
(setq cider-popup-stacktraces t)
(setq cider-popup-stacktraces-in-repl t)

;; CamelCase
(add-hook 'cider-repl-mode-hook 'subword-mode)

;; ac-nrepl - completion source for `auto-complete`
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
    '(add-to-list 'ac-modes 'cider-repl-mode 'cider-mode))

;; Enable minibuffer docs in cider-mode
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; Hide the *nrepl-connection* and *nrepls-server* buffers
(setq nrepl-hide-special-buffers t)

;; Don't show the error popups when not in the REPL
(setq cider-popup-stacktraces nil)

;; Show error popups when in the REPL
(setq cider-repl-popup-stacktraces t)

;; And switch to it automatically
(setq cider-auto-select-error-buffer t)

;; Shoe me dat port
(setq nrepl-buffer-name-show-port t)

;; Make the REPL output commented
(setq cider-repl-result-prefix ";; => ")

;; Suppress line endings (^M) in windows
(when (eq 'windows-nt system-type)
  (defun remove-dos-eol ()
    "Do not show ^M in files containing mixed UNIX and DOS line endings."
    (interactive)
    (setq buffer-display-table (make-display-table))
    (aset buffer-display-table ?\^M []))
  (add-hook 'cider-repl-mode-hook 'remove-dos-eol))

;; Haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

;; hslint on the command line only likes this indentation mode;
;; alternatives commented out below.
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;; Ignore compiled Haskell files in filename completions
(add-to-list 'completion-ignored-extensions ".hi")

;; hippie expand - don't try to complete with file names
(setq hippie-expand-try-functions-list (delete 'try-complete-file-name hippie-expand-try-functions-list))
(setq hippie-expand-try-functions-list (delete 'try-complete-file-name-partially hippie-expand-try-functions-list))

(setq ido-use-filename-at-point nil)

;; Save here instead of littering current directory with emacs backup files
(setq backup-directory-alist `(("." . "~/.saves")))

;; Use Cygwin stuff (this doesn't work that well so I'm leaving it
;; commented for now
;; (add-to-list 'exec-path "C:/cygwin/bin")

;; Sets your shell to use cygwin's bash, if Emacs finds it's running
;; under Windows and c:\cygwin exists. Assumes that C:\cygwin\bin is
;; not already in your Windows Path (it generally should not be).

;; (let* ((cygwin-root "c:/cygwin")
;;        (cygwin-bin (concat cygwin-root "/bin")))
;;   (when (and (eq 'windows-nt system-type)
;;   	     (file-readable-p cygwin-root))
    
;;     (setq exec-path (cons cygwin-bin exec-path))
;;     (setenv "PATH" (concat cygwin-bin ";" (getenv "PATH")))
    
;;     ;; By default use the Windows HOME.
;;     ;; Otherwise, uncomment below to set a HOME
;;     ;;      (setenv "HOME" (concat cygwin-root "/home/eric"))
    
;;     ;; NT-emacs assumes a Windows shell. Change to bash.
;;     (setq shell-file-name "bash")
;;     (setenv "SHELL" shell-file-name) 
;;     (setq explicit-shell-file-name shell-file-name)
    
;;     ;; This removes unsightly ^M characters that would otherwise
;;     ;; appear in the output of java applications.
;;     (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)))
