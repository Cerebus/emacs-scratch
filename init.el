;;; This is my emacs.  There are others like it, but this one is mine.
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;;; Do this early b/c trust management.
(require 'my-auth)

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

;; TODO - need a keymapping that works in Alacritty, iTerm2, xterm, X, and NSwindow
;; ITerm2: remap modifiers option <--> command (preferences, key, remap modifiers)
(when (eq system-type 'darwin)
  (setq ns-command-modifier 'meta
	ns-option-modifier 'super
	ns-control-modifier 'control
	ns-function-modifier 'hyper))

;;; Environmentals.
(unless (package-installed-p 'exec-path-from-shell)
  (package-install 'exec-path-from-shell))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(setenv "EDITOR" "emacsclient")

;;; Globalism
(electric-pair-mode 1)
(electric-indent-mode 1)
(electric-layout-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(auto-insert-mode t)

(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'text-mode-hook #'flyspell-mode)

;;; Theme parking
(setq-default mode-line-format '("%e" mode-line-front-space
 (:propertize
  ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote)
  display
  (min-width
   (5.0)))
 mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
 (:eval (if (project-current) (project-name (project-current)) ""))
 (vc-mode vc-mode)
 "  " mode-line-modes mode-line-misc-info mode-line-end-spaces))

(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t
      modus-themes-syntax '(alt-syntax)
      modus-themes-region '(accented bg-only)
      )

(load-theme 'modus-operandi t)

;;; Key issues
(defvar my/keymap :doc "My leader map")
(defvar my/leader-key "C-c m")
(keymap-global-set my/leader-key my/keymap)

;;; Helpers

(defun my/customize-add-to-list (sym value)
  "Add to a customizable variable's list"
  (customize-set-variable sym (add-to-list sym value)))

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

;; Apps
(require 'my-org)			; Keep this relatively early.
(require 'my-email)
(require 'my-zettel)

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
(require 'my-diagramming)
(require 'my-markdown)
(require 'my-docker)
(require 'my-json)
(require 'my-pdf)
(require 'my-rego)
(require 'my-tex)
(require 'my-cmake)
