;; This is where your customizations should live

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
(require 'ox-jekyll)
(require 'org-octopress)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Always show line numbers
(global-linum-mode t)

;; Gotta have my font
(set-face-attribute 'default nil :font (if (eq system-type 'darwin)
                                           "Source Code Pro-14"
                                         "Source Code Pro Medium-11"))

;; Shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; Themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")

;; Uncomment this to increase font size
;; (set-face-attribute 'default nil :height 140)

;; Theme
(load-theme 'leuven t)

;; Fontify the whole line for headings (with a background color)
;; (Specific to leuven theme)
(setq org-fontify-whole-heading-line t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        Org-mode START                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Org files go here
(setq org-directory "~/org")

;; PlantUML - Get it if we got it
(let ((plantuml-jar-path "/usr/local/Cellar/plantuml/8002/plantuml.8002.jar"))
  (if (file-exists-p plantuml-jar-path)
      (setq org-plantuml-jar-path plantuml-jar-path)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . T)
   (java . t)
   (haskell . t)
   (plantuml . t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           MobileOrg            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Link to Dropbox
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull (concat org-directory "/flagged.org"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           Org-agenda           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Keybindings
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;; Track these files
(setq org-agenda-files (mapcar (lambda (f)
                                 (concat org-directory f))
                               (list "/home.org"
                                     "/work.org"
                                     "/education.org"
                                     "/reading.org"
                                     "/print_manager.org"
                                     "/flagged.org"
                                     "/inbox.org"
                                     "/notes.org"
                                     "/journal.org")))

;; Log timestamp when completing todos
(setq org-log-done t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Org-capture           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cc" 'org-capture)

;; Capture templates
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/inbox.org" "Tasks")
             "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ;; TODO: Fix this
        ("n" "Note" entry (file ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         Org-mode END                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Flyspell often slows down editing so it's turned off
(remove-hook 'text-mode-hook 'turn-on-flyspell)

;; Projectile
(projectile-global-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        Clojure/CIDER                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(setq cider-repl-history-file "~/.emacs.d/nrepl-history")

;; Are these still valid?
(setq cider-popup-stacktraces t)
(setq cider-popup-stacktraces-in-repl t)

;; CamelCase
(add-hook 'cider-repl-mode-hook 'subword-mode)

;; TODO: Replace with ac-cider?
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           Haskell                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

;; hslint on the command line only likes this indentation mode;
;; alternatives commented out below.
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;; Ignore compiled Haskell files in filename completions
(add-to-list 'completion-ignored-extensions ".hi")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Misc                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; hippie expand - don't try to complete with file names
(setq hippie-expand-try-functions-list (delete 'try-complete-file-name hippie-expand-try-functions-list))
(setq hippie-expand-try-functions-list (delete 'try-complete-file-name-partially hippie-expand-try-functions-list))

(setq ido-use-filename-at-point nil)

;; Save here instead of littering current directory with emacs backup files
(setq backup-directory-alist `(("." . "~/.saves")))
