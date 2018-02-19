;; ------------------------------------------
;; -- Allow for different startup directory  --
;; -- Startup with: emacs -q -l some-file
;; ------------------------------------------

;; PLEASE REVIEW IF CLOJURE OR JULIA ARE NOT WORKING
;; CBD:: comment/uncomment (load "ess-site") in order to speed up when not using clojure or julia
;; Search for CBD

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
                      ;; From Balaji S. Srinivasan, Standford
                      ansi-color
                      cl
                      ffap
                      stylus-mode
                      smooth-scrolling
                      whitespace
                      ;; From emacs-live
                      better-defaults
                      dash
                      find-file-in-project
                      idle-highlight-mode
                      magit
                      projectile
                      smex
                      ;; From CBD
                      ;; help and ido are choices: Though both are loaded, only one should configured.
                      helm
                      ido-ubiquitous
                      flx-ido
                      use-package
                      ;; auto-complete and company are choices: Though both are loaded, only one should configured.
                      auto-complete
                      company
                      undo-tree
                      ;; smartparens and paredit are choices: Though both are loaded, only one should configured.
                      paredit
                      paredit-everywhere
                      smartparens
                      rainbow-delimiters
                      ;; Javascript/HTML
                      jade-mode
                      sws-mode
                      js3-mode
                      nodejs-repl
                      ;; Clojure be sure to include the following in profiles.clj: {:user {:plugins [[cider/cider-nrepl "0.8.2"]]}}
                      cider
                      arduino-mode
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

;; From Balaji S. Srinivasan, Standford
(require 'uniquify)
(require 'dired-x)
(require 'compile)

;; ---------------------
;; -- Global Settings --
;; ---------------------

(menu-bar-mode -1)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq column-number-mode t)
(setq inhibit-startup-message t)
(setq save-abbrevs nil)
(setq show-trailing-whitespace t)
(setq suggest-key-bindings t)
(setq vc-follow-symlinks t)
;(normal-erase-is-backspace-mode 1)
(global-set-key "\M-?" 'help)
(global-set-key "\M-\/" 'help-command)
;;(mouse-wheel-mode t)
(global-hl-line-mode t)

;; ------------
;; -- General Macros --
;; ------------

(setq ns-alternate-modifier 'meta) ;set Mac's Fn key to Hyper
(setq ns-command-modifier 'super) ;set Mac's Fn key to Hyper  ;; Not working on OSx 10.9 , but reminder to find a fix

(load "defuns-config.el")
(fset 'align-equals "\C-[xalign-regex\C-m=\C-m")
(global-set-key "\M-=" 'align-equals)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c;" 'comment-or-uncomment-region)
(global-set-key "\M-n" 'next5)
(global-set-key "\M-p" 'prev5)
(global-set-key "\M-o" 'other-window)
(global-set-key "\M-i" 'back-window)
(global-set-key "\C-z" 'zap-to-char)
(global-set-key "\C-h" 'backward-delete-char)
(global-set-key "\M-d" 'delete-word)
(global-set-key "\M-h" 'backward-delete-word)
(global-set-key "\M-u" 'zap-to-char)


;; ---------------------------
;; -- ido configuration --
;; ---------------------------
(ido-mode t)
(ido-ubiquitous-mode t)


;; ---------------------------
;; -- helm configuration -- 
;; ---------------------------
;;
;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
;;
;; (require 'helm-config)
;; (helm-mode 1)

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
;;(require 'smartparens-config)
(smartparens-global-strict-mode)
(show-smartparens-global-mode)
(add-hook 'minibuffer-setup-hook 'smartparens-strict-mode)
(add-hook 'clojure-mode-hook #'smartparens-strict-mode)
(add-hook 'ess-mode-hook 'smartparens-strict-mode)
(add-hook 'inferior-ess-mode-hook 'smartparens-strict-mode)
(add-hook 'prog-mode-hook 'smartparens-strict-mode)

(defmacro def-pairs (pairs)
  `(progn
     ,@(cl-loop for (key . val) in pairs
                collect
                `(defun ,(read (concat
                                "wrap-with-"
                                (prin1-to-string key)
                                "s"))
                     (&optional arg)
                   (interactive "p")
                   (sp-wrap-with-pair ,val)))))

(def-pairs ((paren        . "(")
            (bracket      . "[")
            (brace        . "{")
            (single-quote . "'")
            (double-quote . "\"")
            (back-quote   . "`")))

(bind-keys
 :map smartparens-mode-map
 ("M-A" . sp-beginning-of-sexp)
 ("M-E" . sp-end-of-sexp)
 ("M-N" . sp-next-sexp)
 ("M-P" . sp-previous-sexp)
 ("M-F" . sp-forward-sexp)
 ("M-B" . sp-backward-sexp)

 ("ESC <right>" . sp-forward-slurp-sexp) ;; Same as M-shift <right>
 ("ESC <left>"  . sp-forward-barf-sexp)  ;; Same as M-shift <left>
 ("M-f"  . sp-backward-slurp-sexp)       ;; Same as M-<right>
 ("M-b"  . sp-backward-barf-sexp);; Same as M-<right>

 ("C-c ("  . wrap-with-parens)
 ("C-c ["  . wrap-with-brackets)
 ("C-c {"  . wrap-with-braces)
 ("C-c '"  . wrap-with-single-quotes)
 ("C-c \"" . wrap-with-double-quotes)
 ("C-c _"  . wrap-with-underscores)
 ("C-c `"  . wrap-with-back-quotes)

 ("M-[ " . sp-backward-unwrap-sexp)
 ("M-]"  . sp-unwrap-sexp)

 ("C-M-w" . sp-copy-sexp)
 ("C-M-k" . sp-kill-sexp)
 ("C-k"   . sp-kill-hybrid-sexp)
 ("M-k"   . sp-backward-kill-sexp)

 ("M-<backspace>" . backward-kill-word)
 ("C-<backspace>" . sp-backward-kill-word)
 ([remap sp-backward-kill-word] . backward-kill-word)

 ;; Not tested
 ;; ("M-S-<down>" . sp-down-sexp)
 ;; ("M-S-<up>"   . sp-up-sexp)
 ;; ("M-<down>" . sp-backward-down-sexp)
 ;; ("M-<up>"   . sp-backward-up-sexp)
 )



(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; -----------------------------------------
;; -- Rainbow-delimiters-mode configuration --
;; -----------------------------------------
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
;; NOT SURE WHY 'global-rainbow-delimiters' IS NOT WORKING!!!
;; (global-rainbow-delimiters-mode)


;; -----------------------------------------
;; -- Undo-tree--mode configuration --
;; -----------------------------------------
(require 'undo-tree)
(global-undo-tree-mode)

;; -----------------------------------------
;; -- Projectile-global-mode configuration --
;; -----------------------------------------
(projectile-global-mode)

;; -----------------------------------------
;; -- Smex configuration --
;; -----------------------------------------
(autoload 'smex "smex"
    "Smex is a M-x enhancement for Emacs, it provides a convenient interface to
your recently and most frequently used commands.")

(global-set-key (kbd "M-x") 'smex)


;; -----------------------------------------
;; -- Clipboard configuration --
;; -----------------------------------------
(setq x-select-enable-clipboard t)
(defun yank-to-x-clipboard ()
  (interactive)
  (if (region-active-p)
        (progn
          (shell-command-on-region (region-beginning) (region-end) "xsel -i -b")
          (message "Yanked region to clipboard!")
          (deactivate-mark))
    (message "No region active; can't yank to clipboard!")))

(define-key global-map (kbd "M-W") 'yank-to-x-clipboard)

;; Allow C-y to be used to copy and paste from OS X
;;(defun copy-from-osx ()
;;    (shell-command-to-string "pbpaste"))
;;(defun paste-to-osx (text &optional push)
;;  (let ((process-connection-type nil))
;;    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
;;      (process-send-string proc text)
;;      (process-send-eof proc))))
;;(setq interprogram-cut-function 'paste-to-osx)
;;(setq interprogram-paste-function 'copy-from-osx)


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
(load "ess-site")

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
(add-hook 'pixie-mode-hook #'inf-clojure-minor-mode)

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



;; ---------------------------
;; -- Octave Mode configuration --
;; ---------------------------
(setq auto-mode-alist (cons '("\\.m$" . octave-mode) auto-mode-alist))
(add-hook 'octave-mode-hook
          (lambda ()
            ;; Help mode interfers with global C-h binding
            (local-unset-key "\C-h" )
            (local-set-key "\C-c\C-b" 'octave-send-buffer)
            (local-set-key "\C-c\C-r" 'octave-send-region)
            (local-set-key "\C-ch"   'octave-help)
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))

(add-hook 'inferior-octave-mode-hook
          (lambda ()
            ;; Help mode interfers with global C-h binding
            (local-unset-key "\C-h" )
            (local-set-key "\C-ch"   'octave-help)))


(message "Let's get started...")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (json-mode use-package aggressive-indent smartparens cider undo-tree stylus-mode smooth-scrolling smex rainbow-delimiters projectile pixie-mode paredit-everywhere nodejs-repl magit js3-mode jade-mode ido-ubiquitous idle-highlight-mode helm flx-ido find-file-in-project company better-defaults auto-complete atom-dark-theme arduino-mode ample-zen-theme ample-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

