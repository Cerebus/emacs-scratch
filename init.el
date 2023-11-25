;;; This is my emacs.  There are others like it, but this one is mine.
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;;; Do this early b/c trust management.
(require 'my-auth)

;;; Package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)

(package-initialize)

;; Make package-vc available to use-package
(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))

;;; Housekeeping
(setq custom-file (concat user-emacs-directory "custom.el"))

(when (file-exists-p custom-file)
  (load custom-file))

(unless (display-graphic-p)
  (xterm-mouse-mode 1))

;; TODO - need a keymapping that works in Alacritty, X, and NSwindow
(when (and (eq system-type 'darwin)) ; (display-graphic-p))
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'option))

;;; Environmentals.
(unless (package-installed-p 'exec-path-from-shell)
  (package-install 'exec-path-from-shell))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;; Globalism
(electric-pair-mode 1)
(electric-indent-mode 1)
(electric-layout-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(global-display-line-numbers-mode 1)
(auto-insert-mode t)

;;; Server -- Gonna use server-mode alla time, but I want it to exit
;;; when I exit the terminal
(server-start)
(bind-key (kbd "C-x C-c") #'save-buffers-kill-emacs)

(add-hook 'before-save-hook #'delete-trailing-whitespace)

;;; Theme parking
(setq-default mode-line-format '("%e" mode-line-front-space
 (:propertize
  ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote)
  display
  (min-width
   (5.0)))
 mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
 (:eval (project-name (project-current)))
 (vc-mode vc-mode)
 "  " mode-line-modes mode-line-misc-info mode-line-end-spaces))

(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t
      modus-themes-syntax '(alt-syntax)
      modus-themes-region '(accented bg-only)
      )

(load-theme 'modus-operandi t)

;;; Modules

;; Install and configure core
(require 'my-consult)
(require 'my-vertico)
(require 'my-marginalia)
(require 'my-orderless)
(require 'my-embark)
(require 'my-corfu)
(require 'my-cape)
(require 'my-combobulate)
(require 'my-helpful)
(require 'my-ace-window)
(require 'my-visual)

;; IDE all the things
(require 'my-eshell)
(require 'my-ide)
(require 'my-hl-todo)

;; language modes
(require 'my-python)
(require 'my-csv)
(require 'my-yaml)
(require 'my-jinja2)
(require 'my-jinja-yaml)
(require 'my-golang)
(require 'my-plantuml)
(require 'my-markdown)
(require 'my-docker)
(require 'my-json)
(require 'my-pdf)
(require 'my-rego)

;; Apps
(require 'my-email)
(require 'my-org)
