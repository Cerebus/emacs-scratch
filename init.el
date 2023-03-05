;;; Move the custom file
(setq custom-file (concat user-emacs-directory "custom.el"))

(when (file-exists-p custom-file)
  (load custom-file))

;; Add my modules to load-path
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;; Terminal vs not
(unless (display-graphic-p)
  (xterm-mouse-mode 1))

;;; Globalism
(electric-pair-mode 1)
(electric-indent-mode 1)
(electric-layout-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)

;;; Theme parking
(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t
      modus-themes-syntax '(alt-syntax)
      modus-themes-region '(bg-only no-extend)
      )

(load-theme 'modus-operandi t)

;;; Some key conventions
;; (keyboard-translate (kbd "<deletechar>") (kbd "DEL"))

;;; Package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;; Make package-vc available to use-package
(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))

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

;; IDE all the things
(require 'my-ide)

;; language modes
(require 'my-python)
