;;; This is my emacs.  There are others like it, but this one is mine.

;;; Package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

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

;;; Path maintenance.
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

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
(global-visual-line-mode 1)
(global-display-line-numbers-mode 1)

;;; Theme parking
(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t
      modus-themes-syntax '(alt-syntax)
      modus-themes-region '(bg-only no-extend)
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
(require 'my-tabspaces)

;; IDE all the things
(require 'my-ide)

;; language modes
(require 'my-python)
(require 'my-yaml)
(require 'my-jinja2)
(require 'my-jinja-yaml)
