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

;; UI SECTION

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq-default cursor-type 'bar)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(set-frame-parameter nil 'alpha-background 80)

;; Fonts
(set-face-attribute 'default nil :font "Hack Nerd Font Mono" :height 110)
(set-face-attribute 'fixed-pitch nil :family "Hack Nerd Font Mono")
(set-face-attribute 'variable-pitch nil :family "Hack Nerd Font Mono" :height 110)

;; Nerd Icons
(require 'nerd-icons)
(require 'nerd-icons-dired)
(add-hook 'dired-mode-hook #'nerd-icons-dired-mode)

;; Mode line
(require 'doom-modeline)
(doom-modeline-mode 1)

;; Load theme
(use-package doom-themes)
(use-package fleetish-theme
   :init (load-theme 'fleetish))

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

;; Eglot and company config
(require 'company)
(global-company-mode)

(setq eldoc-echo-area-use-multiline-p nil)

(use-package eglot
  :config
  (add-hook 'haskell-mode-hook 'eglot-ensure)
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

;; Add GHCUP to path for haskell language server 
(add-to-list 'exec-path "~/.ghcup/bin")

;; Add C# tools to exec path
(add-to-list 'exec-path "~/.dotnet/tools")

;; Lisp config
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq-default indent-tabs-mode nil)

;; C, C++ config
(setq-default c-basic-offset 4)

;; Open config file
(defun open-config-file ()
  "Open Emacs configuration file."
  (interactive)
  (find-file (expand-file-name "~/.config/emacs/init.el")))

(global-set-key (kbd "C-c c") 'open-config-file)
