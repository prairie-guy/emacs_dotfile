;; ------------------------------------------
;; -- Allow for different startup directory  --
;; -- Startup with: emacs -q -l some-file
;; ------------------------------------------
;; PLEASE REVIEW IF CLOJURE OR JULIA ARE NOT WORKING
;; CBD:: comment/uncomment (load "ess-site") in order to speed up when not using clojure or julia
;; Search for CBD


;; Customize .emacs.d and emacs -l to load an alternative directory
(setq args command-line-args)
(setq my-init-directory user-emacs-directory)
(setq init-file nil)

(while args
  (if (string-equal "-l" (car args))
      (setq init-file (car (cdr args))))
  (pop args))

(if init-file
    (if (file-name-directory init-file)
        (setq my-init-directory (file-name-directory init-file))
      (error "Error: when indicating a startup file with 'emacs -l' specify a directory in it's path, not just a filename.")))

(if (not (string-equal my-init-directory "~/.emacs.d/" ))
    (add-to-list 'load-path  my-init-directory))

(add-to-list 'load-path  (concat my-init-directory "custom.d/"))
(setq package-user-dir (concat my-init-directory "elpa/"))


;; ---------------------
;; -- Package Settings --
;; ---------------------

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(
		      ansi-color
		      smooth-scrolling         ;; Provide smooth scrolling
                      whitespace               ;; Toggles on whitespace
		      magit
		      smex
		      use-package
		      ;; helm and ido are choices: Both are loaded. Only one should configured.
                      helm
		      flx-ido
		      company
                      undo-tree
		      smartparens
                      rainbow-delimiters
                      ;; Javascript/HTML
                      jade-mode
                      sws-mode
                      js3-mode
                      nodejs-repl
                      ;; clojure be sure to include the following in profiles.clj: {:user {:plugins [[cider/cider-nrepl "0.8.2"]]}}
                      cider
		      aggressive-indent
                      ;; Julia
                      julia-mode
                      ;; Several themes
                      ample-zen-theme
                      ample-theme
                      atom-dark-theme
		      ;; Python
		      elpy
                      )
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; ---------------------
;; -- Global Settings --
;; ---------------------

;;(put 'downcase-region 'disabled nil)
;;(put 'upcase-region 'disabled nil)
;;(setq save-abbrevs nil)
;;(setq show-trailing-whitespace t)
;;(setq suggest-key-bindings t)
;;(setq vc-follow-symlinks t)
;;(normal-erase-is-backspace-mode 1)
;;(global-set-key "\M-\/" 'help-command)
;;(mouse-wheel-mode t)
;;(global-hl-line-mode t)
(menu-bar-mode -1) ; Disable menu bar at top of screen
(setq column-number-mode t)
(setq inhibit-startup-message t)
(setq ns-alternate-modifier 'meta);set Mac's Fn key to Hyper
(setq ns-command-modifier 'super);set Mac's Fn key to Hyper  ;; Not working on OSx 10.9 , but reminder to find a fix

;; --------------------------
;; -- Global Key Bindings  --
;; --------------------------

;;(global-set-key "\C-x\C-m" 'execute-extended-command)
(load "defuns-config.el")
(fset 'align-equals "\C-[xalign-regex\C-m=\C-m")
(global-set-key "\M-?" 'help)                          ; Help-Key
(global-set-key "\M-=" 'align-equals)                  ; Aligment-Key
(global-set-key "\C-c;" 'comment-or-uncomment-region)  ; Commment/Uncomment-Key
(global-set-key "\M-n" 'next5)                         ; Next-five-Key
(global-set-key "\M-p" 'prev5)                         ; Previous-five-Key
(global-set-key "\M-o" 'other-window)                  ; Other-window-Key
(global-set-key "\M-i" 'back-window)                   ; Prior-window-Key
(global-set-key "\C-z" 'zap-to-char)                   ; Zap-point-to-char-Key
(global-set-key "\M-d" 'delete-word)                   ; Forward-delete-word-key
(global-set-key "\M-h" 'backward-delete-word)          ; Bacward-delete-word-key


;; ---------------------------
;; -- uniquify --
;; ---------------------------
(require 'uniquify)

;; ---------------------------
;; -- dired-x --
;; ---------------------------
(require 'dired-x)

;; ---------------------------
;; -- compile --
;; ---------------------------
(require 'compile)

;; ---------------------------
;; -- smooth-scrolling --
;; ---------------------------
(smooth-scrolling-mode t)

;; ---------------------------
;; -- ido configuration --
;; ---------------------------
;; (ido-mode t)
;; (setq ido-everywhere t)
;; (setq ido-enable-flex-matching t)

;; ---------------------------
;; -- helm configuration -- 
;; ---------------------------
(require 'helm)
(require 'helm-config)
(helm-mode t)
(global-set-key (kbd "C-c h") 'helm-command-prefix)                ; Helm-command (Changed to "C-c h"); "C-c h i" opens i-menu. VERY USEFUL
(global-set-key (kbd "M-x") 'helm-M-x)                             ; Choose this or "M-x" for 'smex. NOT BOTH
(global-unset-key (kbd "C-x c"))                                   ; Cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action); Rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)  ; Make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action)             ; List actions using C-z
(global-set-key (kbd "M-y") 'helm-show-kill-ring)                  ; Uber version of kill-ring
(global-set-key (kbd "C-x b") 'helm-mini)                          ; Helm version of searching buffer
(global-set-key (kbd "C-x C-f") 'helm-find-files)                  ; Helm file navigation
(setq helm-M-x-fuzzy-match t)                                      ; Fuzzy matching for helm-M-x
(setq helm-buffers-fuzzy-matching t                                ; Fuzzy matching for buffers
      helm-recentf-fuzzy-match    t
      helm-semantic-fuzzy-match   t
      helm-imenu-fuzzy-match      t)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)
(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)
(helm-autoresize-mode 1)

;; ---------------------------
;; -- company configuration --
;; ---------------------------
(global-company-mode)
(define-key company-active-map (kbd "\C-n") 'company-select-next)
(define-key company-active-map (kbd "\C-p") 'company-select-previous)
(define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
(define-key company-active-map (kbd "<tab>") 'company-complete)

;; ---------------------------
;; -- smartparens configuration --
;; ---------------------------

;; New/simpler/default configuration
(require 'smartparens-config)
(smartparens-global-strict-mode)
(show-smartparens-global-mode)

;; -----------------------------------------
;; -- Rainbow-delimiters-mode configuration --
;; -----------------------------------------
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; -----------------------------------------
;; -- Undo-tree--mode configuration --
;; -----------------------------------------
(require 'undo-tree)
(global-undo-tree-mode)

;; -----------------------------------------
;; -- Smex configuration --
;; -----------------------------------------
(autoload 'smex "smex"
  "Smex is a M-x enhancement for Emacs, it provides a convenient interface to
your recently and most frequently used commands.")
;; Choose this or "M-x" for 'helm
;; (global-set-key (kbd "M-x") 'smex)


;; -----------------------------------------
;; -- Misc configuration --
;; -----------------------------------------

;; Change default spell-checking program
(setq-default ispell-program-name "aspell")

;; Set my email address
;; Setup mail-transport agent to accept smtp traffic, i.e., postfix on os x
(setq user-mail-address "cdaniels@nandor.net")

;; -----------------------------------------
;; -- Themes --
;; -----------------------------------------
;; Make sure that this is in .bashrc: export TERM="xterm-256color"

;;ample-zen-theme
;;ample-theme
;;atom-dark-theme
(load-theme 'ample-zen t)
(enable-theme 'ample-zen)

;; -------------------------------------------
;; -- Ess Mode Configuration (R and Julia)---
;; -------------------------------------------
;; Ess is not part of package management system, so need to run 'setup.sh' in order to install ess.d.
(add-to-list 'load-path  (concat my-init-directory "ess.d/lisp"))

;; CBD comment/uncomment (load "ess-site") in order to speed up when not using clojure or julia
;;(load "ess-site")

;; OK TO LEAVE COMMENTED
;;(require 'ess-eldoc)
(setq-default inferior-R-args "--no-restore-history --no-save ")
(setq ess-default-style 'RRR)
(setq ess-tab-complete-in-script t)                 ;; Autocomplete in .R files M-tab
(setq ess-first-tab-never-complete t)
(setq ess-eldoc-show-on-symbol t)
(setq ess-eldoc-abbreviation-style 'strong)

(defun run-R()
  (interactive)
  (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
      (progn
        (delete-other-windows)
        (setq w1 (selected-window))
        (setq w1name (buffer-name))
        (setq w2 (split-window w1 nil t))
        (R)
;;        (set-window-buffer w1 "*R*")    ; R on the left
;;        (set-window-buffer w2 w1name))))
        (set-window-buffer w2 "*R*")
        (set-window-buffer w1 w1name))))

;;(setq inferior-julia-program-name "/usr/local/bin/julia")
(defun run-julia()
  (interactive)
  (if (not (member "*julia*" (mapcar (function buffer-name) (buffer-list))))
      (progn
        (delete-other-windows)
        (setq w1 (selected-window))
        (setq w1name (buffer-name))
        (setq w2 (split-window w1 nil t))
        (julia)
        (set-window-buffer w2 "*julia*")
        (set-window-buffer w1 w1name))))


;; -------------------------------------------
;; -- Clojure(cider) Mode Configuration    ---
;; -------------------------------------------
(add-hook 'cider-mode-hook #'eldoc-mode)      ;; Enable eldoc in Clojure buffers
(setq nrepl-hide-special-buffers t)           ;; Hide the *nrepl-connection* and *nrepl-server* buffers from appearing in some buffer switching commands
(setq cider-show-error-buffer nil)
;;(setq cider-show-error-buffer 'only-in-repl)


;; -------------------------------------------
;; -- Clojure (Other) Mode Configuration    ---
;; -------------------------------------------
(require 'aggressive-indent)
(global-aggressive-indent-mode 1)

(load "js-config.el")
(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))


;; Python
;; ---------------------------
;; -- Python configuration --
;; ---------------------------
(elpy-enable)
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

(message "Let's get started...")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (simpleclip use-package undo-tree sws-mode smooth-scrolling smex smartparens rainbow-delimiters nodejs-repl magit julia-mode js3-mode jade-mode helm flx-ido elpy cider atom-dark-theme ample-zen-theme ample-theme aggressive-indent))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
