;; ----------------
;; CUSTOM VARIABLES & FACES
;; ----------------

(custom-set-variables
 '(custom-safe-themes t)
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(catppuccin-theme company dashboard doom-modeline
					  exec-path-from-shell go-mode haskell-mode
					  ligature magit nerd-icons-completion
					  nerd-icons-dired python-mode rust-mode sly)))
(custom-set-faces
 '(default ((t (:background nil)))))

;; ----------------
;; PACKAGE MANAGEMENT
;; ----------------


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

;; ------------
;; ENVIRONMENT VARIABLES
;; ------------

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; ------------
;; UI/THEMING
;; ------------

;; Window
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(set-frame-parameter nil 'alpha-background 100)

;; Bars
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Line numbers, Cursor
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq-default cursor-type 'bar)

;; Audible bell
(setq ring-bell-function 'ignore)

;; Fonts
(set-face-attribute 'default nil :font "FiraCode Nerd Font" :height 100)
(set-face-attribute 'fixed-pitch nil :family "FiraCode Nerd Font" :height 100)
(set-face-attribute 'variable-pitch nil :family "FiraCode Nerd Font" :height 100)

;; Font ligatures
(use-package ligature)
(ligature-set-ligatures 't '("www"))
(ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                     ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                     "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                     "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                     "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                     "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                     "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                     "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                     "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                     "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
(global-ligature-mode 't)

;; Nerd Icons
(use-package nerd-icons)
(use-package nerd-icons-dired)
(add-hook 'dired-mode-hook #'nerd-icons-dired-mode)

;; Mode line
(use-package doom-modeline)
(doom-modeline-mode 2)
(setq doom-modeline-icon t)

;; Install themes
(use-package catppuccin-theme)
(setq catppuccin-flavor 'macchiato)
(load-theme 'catppuccin t)

;; Dashboard
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  :custom
  ;; Logo and center content
  (dashboard-startup-banner (expand-file-name "logo-2.txt" user-emacs-directory))
  ;; (dashboard-startup-banner 'official)
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

;; Compilation window settings
(setq display-buffer-alist
      '(("\\*compilation\\*"
         (display-buffer-reuse-window display-buffer-at-bottom)
         (window-height . 0.3))))

;; --------------------
;; KEYBINDINGS & MACROS
;; --------------------

;; Custom macros and keybindings to scroll the buffer
(defun move-buffer-up-one-line ()
  "Scroll the buffer content up by one line without moving the cursor."
  (interactive)
  (scroll-down-line 1))

(defun move-buffer-down-one-line ()
  "Scroll the buffer down one line."
  (interactive)
  (scroll-up-line 1))

;; Backward/forward paragraph key binding
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-]") 'move-buffer-up-one-line)
(global-set-key (kbd "M-[") 'move-buffer-down-one-line)

;; Compile
(global-set-key (kbd "C-c C-x") 'compile)

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

;; --------------------
;; LSP & LANGUAGE MODE
;; --------------------

;; Indentation
(setq-default indent-tabs-mode t)
(setq-default tab-width 4)

;; Language modes
(use-package go-mode
  :custom
  (add-hook 'before-save-hook #'gofmt-before-save))

(use-package python-mode
  :custom
  (python-shell-interpreter "python3"))

(use-package rust-mode
  :custom
  (setq rust-format-on-save t))

(use-package haskell-mode)


;; Lisp config
(use-package sly)
(setq inferior-lisp-program "/usr/bin/sbcl")

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
  (add-hook 'rust-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  (eglot-confirm-server-initiated-edits nil))

(define-key eglot-mode-map (kbd "C-c <tab>") #'company-complete)
(define-key eglot-mode-map (kbd "C-c e f n") #'flymake-goto-next-error)
(define-key eglot-mode-map (kbd "C-c e f p") #'flymake-goto-prev-error)
(define-key eglot-mode-map (kbd "C-c e r") #'eglot-rename)

;; Nerd icons in completion window
(use-package nerd-icons-completion
  :config
(nerd-icons-completion-mode))

;; Dired mode
(setq dired-listing-switches "-aBhl --group-directories-first")

;; Magit
(use-package magit)
