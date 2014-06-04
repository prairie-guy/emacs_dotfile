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
                      dash auto-complete rainbow-delimiters ace-jump-mode projectile 
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

;; ------------
;; -- General Macros --
;; ------------
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
;; -- JS Mode configuration --
;; ---------------------------
(load "js-config.el")
;;(add-to-list 'load-path "~/.emacs.d/jade-mode") ;; github.com/brianc/jade-mode
;;(require 'sws-mode)
;;(require 'jade-mode)
(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))


;; ---------------------------
;; -- Paredit configuration --
;; ---------------------------

(require 'paredit)
(define-key paredit-mode-map (kbd "C-M-]") 'paredit-forward-barf-sexp)
(define-key paredit-mode-map (kbd "C-M-[") 'paredit-backward-barf-sexp)
(define-key paredit-mode-map (kbd "M-]") 'paredit-forward-slurp-sexp)
(define-key paredit-mode-map (kbd "M-[") 'paredit-backward-slurp-sexp)
(require 'paredit-everywhere)
(add-hook 'prog-mode-hook 'paredit-everywhere-mode)

;; Always show matching parens
(show-paren-mode 1)

;; ---------------------------
;; -- Ace Jump configuration --
;; ---------------------------

(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)

(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)


;; -----------------------------------------
;; -- Rainbow-delimiters-mode configuration --
;; -----------------------------------------

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)

;; -----------------------------------------
;; -- Projectile-global-mode configuration --
;; -----------------------------------------
;;(require 'undo-tree)

;; -----------------------------------------
;; -- Projectile-global-mode configuration --
;; -----------------------------------------

(projectile-global-mode)

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
(defun copy-from-osx ()
    (shell-command-to-string "pbpaste"))
(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))
(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)


;; -----------------------------------------
;; -- Misc configuration --
;; -----------------------------------------

;; Change default spell-checking program
(setq-default ispell-program-name "aspell")

;; Set my email address
;; Setup mail-transport agent to accept smtp traffic, i.e., postfix on os x
(setq user-mail-address "cdaniels@nandor.net")


;; -----------------------------------------
;; -- Custom Variable configuration --
;; -----------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((epa-file-encrypt-to arthur@ulfeldt\.com) (whitespace-line-column . 80) (lexical-binding . t)))))


;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:inherit nil :stipple nil :background "color-234" :foreground "unspecified-fg" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default")))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit autoface-default :strike-through nil :underline nil :slant normal :weight normal :height 120 :width normal :family "monaco"))))
 '(column-marker-1 ((t (:background "red"))))
 '(diff-added ((t (:foreground "cyan"))))
 '(flymake-errline ((((class color) (background light)) (:background "Red"))))
 '(font-lock-comment-face ((((class color) (min-colors 8) (background light)) (:foreground "red"))))
 '(fundamental-mode-default ((t (:inherit default))))
 '(highlight ((((class color) (min-colors 8)) (:background "white" :foreground "magenta"))))
 '(isearch ((((class color) (min-colors 8)) (:background "yellow" :foreground "black"))))
 '(linum ((t (:foreground "black" :weight bold))))
 '(region ((((class color) (min-colors 8)) (:background "white" :foreground "magenta"))))
 '(secondary-selection ((((class color) (min-colors 8)) (:background "gray" :foreground "cyan"))))
 '(show-paren-match ((((class color) (background light)) (:background "black"))))
 '(vertical-border ((t nil))))
