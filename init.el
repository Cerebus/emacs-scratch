;;; init.el --- Emacs init                           -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Timothy Miller

;; Author: Timothy Miller <timothymiller@hotplate.lan>
;; Keywords: local

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is my init.el.  There are many like it but this one is mine.

;; My init.el is my best friend.  It is my life.  I must master it as
;; I master my life.

;; My init.el, without me, is useless.  Without my init.el, I am
;; useless.  I must write my init.el true.  I must load faster than my
;; rival who is trying to best me.  I must outrace him before he out
;; outrace me.

;; My init.el and myself know that what counts in this struggle is not
;; the lines we write, the showiness of our themes, nor the
;; integrations we make.  We know that it is efficiency that counts.

;; Emacs is human, even as I, because it is my life.  Thus, I will
;; learn it as a brother.  I will learn its syntax, its idioms, its
;; packages, its modes, its keymaps and its defuns.  I will ever guard
;; it against the ravages of feature creep and code rot as I will ever
;; guard my legs, my arms, my eyes and my heart against damage.  I
;; will keep my init.el clean and ready.  We will become part of each
;; other.

;; Before GNU, I swear this creed.  My init.el and myself are the
;; defenders of my productivity.  We are the masters of our
;; environment.  We are the saviors of my life.

;;; Code:
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(when (eq system-type 'darwin)
  (add-to-list 'load-path "/opt/homebrew/share/emacs/site-lisp/mu/mu4e/")) ; brew links it here

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
(setq inhibit-splash-screen t)

;;; Do this early b/c trust management.
(require 'my-auth)

;;; Package management
;; FIXME: migrate to use-package and remove
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Environmentals
(use-package exec-path-from-shell
  :ensure
  :custom
  (exec-path-from-shell-arguments nil)
  :init (exec-path-from-shell-initialize))

(if (string-equal system-type "darwin")
    (customize-set-variable 'dired-use-ls-dired nil))

(setenv "EDITOR" "emacsclient")
(setenv "SSH_AUTH_SOCK" (expand-file-name "~/.gnupg/S.gpg-agent.ssh"))

(when (eq system-type 'darwin)		; TODO: want these or not?
  (setq ns-command-modifier 'meta
	ns-option-modifier 'super
	ns-control-modifier 'control
	ns-function-modifier 'hyper)
  ;; Is there a better hackaround?
  (define-key key-translation-map (kbd "<M-mouse-1>")  (kbd "<mouse-2>")))

;; Modalities
(xterm-mouse-mode 1)
(which-key-mode 1)
(electric-pair-mode 1)
(electric-indent-mode 1)
(electric-layout-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(auto-insert-mode t)

;;; Globalism
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;;; Theme parking
(require 'my-look)

;;; Key issues
(defvar-keymap my/keymap :doc "My leader map")
(defvar my/leader-key "C-c SPC")
(keymap-global-set my/leader-key my/keymap)

;;; Helpers
(defun my/customize-add-to-list (sym value)
  "Add to a customizable variable's list"
  (customize-set-variable sym (add-to-list sym value)))

;;; Modules
(require 'my-emacs)			; Core

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-vc-selected-packages
   '((vc-use-package :vc-backend Git :url
		     "https://github.com/slotThe/vc-use-package"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
