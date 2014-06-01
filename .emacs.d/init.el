;; ------------------------------------------
;; -- Allow for different startup directory  --
;; -- Startup with: emacs -q -l some-file  
;; ------------------------------------------

(setq args command-line-args)
(setq my-init-directory user-emacs-directory)
(setq init-file nil)

(while args
  (if (string-equal "-l" (car args)) (setq init-file (car (cdr args))))
  (pop args))

(if init-file
    (if (file-name-directory init-file)
        (setq my-init-directory (file-name-directory init-file))
      (error "Error: when indicating a startup file with 'emacs -l' specify a directory in it's path, not just a filename.")))

(message my-init-directory)

(add-to-list 'load-path  my-init-directory)
(add-to-list 'load-path  (concat my-init-directory "custom.d/")) 
(setq package-user-dir (concat my-init-directory "elpa"))

;; ---------------------
;; -- Package Settings --
;; ---------------------

(require 'package)

(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(better-defaults
		      paredit paredit-everywhere
		      idle-highlight-mode find-file-in-project smex ido-ubiquitous magit
		      clojure-mode dash cider auto-complete ac-nrepl
		      rainbow-delimiters ace-jump-mode projectile
		      ;; From Balaji S. Srinivasan, Standford
		      jade-mode sws-mode stylus-mode
		      cl ffap ansi-color recentf whitespace smooth-scrolling column-marker
		      ;; From CBD
		      undo-tree
		      )
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; From Balaji S. Srinivasan, Standford
(require 'uniquify)
(require 'dired-x)
(require 'compile)

;; ---------------------
;; -- Global Settings --
;; ---------------------

(ido-mode t)
(menu-bar-mode -1)
;(normal-erase-is-backspace-mode 1)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq column-number-mode t)
(setq inhibit-startup-message t)
(setq save-abbrevs nil)
(setq show-trailing-whitespace t)
(setq suggest-key-bindings t)
(setq vc-follow-symlinks t)

;; Assigns C-h the 'Delete-key' to C-? the 'Bacskspace-key'
(keyboard-translate ?\C-h ?\C-?)

;; Assigns 'help' to the 'C-\ key'
(keyboard-translate ?\C-\\ ?\C-h)

;; Always show matching parens
(show-paren-mode 1)

;; Allow C-y to be used to copy and paste from OS X
(defun copy-from-osx ()
    (shell-command-to-string "pbpaste"))
(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))
(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;; Change default spell-checking program
(setq-default ispell-program-name "aspell")

;; Set my email address
;; Setup mail-transport agent to accept smtp traffic, i.e., postfix on os x
(setq user-mail-address "cdaniels@nandor.net")
