;; PACKAGE MANAGEMENT

;; Set package repos
(setq package-archives
      '(("MELPA" . "https://melpa.org/packages/")
	("ELPA" . "https://elpa.gnu.org/packages/")))

;; Boostrap use-package
(package-initialize)
(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

;; ENVIRONMENT VARIABLES

;; Get PATH from shell
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
that used by the user's shell.

This is particularly useful under Mac OS X and macOS, where GUI
apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
			  "[ \t\n]*$" "" (shell-command-to-string
					  "$SHELL --login -c 'echo $PATH'"
						    ))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

;; UI/THEMING

;; Window
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(set-frame-parameter nil 'alpha-background 100)

;; Bars
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Line numbers, Cursor
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(setq-default cursor-type 'bar)

;; Audible bell
(setq ring-bell-function 'ignore)

;; Fonts
(set-face-attribute 'default nil :font "Hack Nerd Font Mono" :height 100)
(set-face-attribute 'fixed-pitch nil :family "Hack Nerd Font Mono" :height 100)
(set-face-attribute 'variable-pitch nil :family "Hack Nerd Font Mono" :height 100)

;; Nerd Icons
(use-package nerd-icons)
(use-package nerd-icons-dired)
(add-hook 'dired-mode-hook #'nerd-icons-dired-mode)

;; Mode line
(use-package doom-modeline)
(doom-modeline-mode 2)
;(setq doom-modeline-height 10)     

;; Load theme
(use-package kaolin-themes)
(load-theme 'kaolin-bubblegum)

;; Dashboard
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  :custom
  ;; Logo and center content
  (dashboard-startup-banner 2)
  (dashboard-center-content t)

  ;; Use nerd icons
  (dashboard-display-icons-p t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)

  ;; Specify dashboard items
  (dashboard-items '((recents   . 5)
                     (bookmarks . 5)
                     (projects  . 5))))

(global-set-key (kbd "C-c d") 'dashboard-open)

;; *LSP & LANGUAGE CONFIG*

;; Language modes
(use-package haskell-mode)

;; Lisp config
(use-package sly)
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq-default indent-tabs-mode nil)

;; C, C++ config
(setq-default c-basic-offset 4)

;; Eglot and company config
(use-package company)
(global-company-mode)

(setq eldoc-echo-area-use-multiline-p nil)

(use-package eglot
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (add-hook 'haskell-mode-hook 'eglot-ensure)
  (add-hook 'go-mode-hook 'eglot-ensure)
  :config
  (setq-default eglot-workspace-configuration
                '((haskell
                   (plugin
                    (stan
                     (globalOn . :json-false))))))
  :custom
  (eglot-autoshutdown t)
  (eglot-confirm-server-initiated-edits nil))

(define-key eglot-mode-map (kbd "C-c <tab>") #'company-complete)
(define-key eglot-mode-map (kbd "C-c e f n") #'flymake-goto-next-error)
(define-key eglot-mode-map (kbd "C-c e f p") #'flymake-goto-prev-error)
(define-key eglot-mode-map (kbd "C-c e r") #'eglot-rename);; Nerd icons in completion window
(use-package nerd-icons-completion
  :config
  (nerd-icons-completion-mode))

;; Backups
(setq delete-auto-save-files t)

;; *KEYBINDINGS & MACROS*

;; Backward/forward paragraph key binding
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-[") 'scroll-up-command)
(global-set-key (kbd "M-]") 'scroll-down-command)

;; Open config file macro
(defun open-config-file ()
  "Open Emacs configuration file."
  (interactive)
  (find-file (expand-file-name "~/.config/emacs/init.el")))

(global-set-key (kbd "C-c c") 'open-config-file)

;; Open short terminal buffer
(defun open-term-buf ()
  "Open a short terminal buffer"
  (interactive)
  (let ((current-window (selected-window)))
    (split-window-below)
    (other-window 1)
    (window-resize (selected-window) (- 12 (window-total-height)))
    (ansi-term (getenv "SHELL"))
    (select-window current-window)))

(global-set-key (kbd "C-c t") 'open-term-buf)

;; Eval init.el on save
(defun init-eval ()
  "Evaluate the config file on save."
  (when (string-equal buffer-file-name (expand-file-name "~/.config/emacs/init.el"))
    (eval-buffer)))

(add-hook 'after-save-hook 'init-eval)

;; Swap buffers
(global-set-key (kbd "C-x p") 'window-swap-states)
