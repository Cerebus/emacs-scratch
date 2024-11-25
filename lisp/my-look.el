;;; my-look.el --- Look and feel                     -*- lexical-binding: t; -*-

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
;; GNU General Puqblic License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Deep changes to Emacs look and feel, beyond just theming.

;;; Code:
(setq-default mode-line-format nil)
(setq use-dialog-box nil
      use-file-dialog nil)

(use-package nano-theme
  :init
  (load-theme 'nano t)
  :custom
  (nano-fonts-use t)
  :custom-face
  (nano-mono ((t (:family "RobotoMono Nerd Font Mono"))))
  (nano-mono-alt ((t (:family "FiraCode Nerd Font Mono"))))
  (nano-sans ((t (:family "RobotoMono Nerd Font Propo"))))
  (nano-serif ((t (:family "Roboto Slab"))))
  (nano-italic ((t (:family "Victor Mono"))))
  (tab-bar-tab ((t (:inherit nano-strong))))
  (tab-bar-tab-inactive ((t (:inherit nano-subtle))))
  :vc (:url "https://github.com/rougier/nano-theme"))

(use-package nano-modeline
  :vc (:url "https://github.com/rougier/nano-modeline")
  :custom
  (nano-modeline-position 'nano-modeline-header)
  :config
  (nano-modeline-text-mode 1)
  (defun my/nano-modeline-flymake ()
    "Flymake information."
    (if (and (boundp 'flymake-mode) flymake-mode)
	(propertize (concat (format-mode-line flymake-mode-line-title)
			    " "
			    (format-mode-line flymake-mode-line-counters))
		    'face (nano-modeline-face 'primary))))
  (defun my/nano-modeline-prog-mode (&optional default)
    "Nano line for prog mode with flymake."
    (funcall nano-modeline-position
	     '((nano-modeline-buffer-status) " "
	       (nano-modeline-buffer-name) " "
	       (nano-modeline-git-info))
	     '((my/nano-modeline-flymake) " "
	       (nano-modeline-cursor-position)
	       (nano-modeline-window-dedicated))
	     default))
  (defun my/nano-modeline-org-mode (&optional default)
    (funcall nano-modeline-position
	     '((nano-modeline-buffer-status) " "
	       (nano-modeline-org-buffer-name) " "
	       (nano-modeline-git-info))
	     '((my/nano-modeline-flymake) " "
	       (nano-modeline-cursor-position)
	       (nano-modeline-window-dedicated))
	     default))
  (add-hook 'prog-mode-hook            #'my/nano-modeline-prog-mode)
  (add-hook 'text-mode-hook            #'nano-modeline-text-mode)
  (add-hook 'org-mode-hook             #'my/nano-modeline-org-mode)
  (add-hook 'pdf-view-mode-hook        #'nano-modeline-pdf-mode)
  (add-hook 'mu4e-headers-mode-hook    #'nano-modeline-mu4e-headers-mode)
  (add-hook 'mu4e-view-mode-hook       #'nano-modeline-mu4e-message-mode)
  (add-hook 'elfeed-show-mode-hook     #'nano-modeline-elfeed-entry-mode)
  (add-hook 'elfeed-search-mode-hook   #'nano-modeline-elfeed-search-mode)
  (add-hook 'term-mode-hook            #'nano-modeline-term-mode)
  (add-hook 'xwidget-webkit-mode-hook  #'nano-modeline-xwidget-mode)
  (add-hook 'messages-buffer-mode-hook #'nano-modeline-message-mode)
  (add-hook 'org-capture-mode-hook     #'nano-modeline-org-capture-mode)
  (add-hook 'org-agenda-mode-hook      #'nano-modeline-org-agenda-mode))

;; (use-package almost-mono-themes
;;   :ensure
;;   :init
;;   (load-theme 'almost-mono-white t))

(use-package nano-vertico
  :vc (:url "https://github.com/rougier/nano-vertico")
  :custom
  (nano-vertico-prompt nil)
  :config
  (nano-vertico-mode 1))

;; (use-package telephone-line
;;   :vc (:url "https://github.com/dbordak/telephone-line" :rev :newest)
;;   :commands
;;   (telephone-line-defsegment*)
;;   :custom
;;   (telephone-line-target 'header-line)
;;   (telephone-line-height (* (font-get (face-attribute 'default :font) :size) 2))
;;   (telephone-line-primary-left-separator 'telephone-line-flat)
;;   (telephone-line-secondary-left-separator 'telephone-line-identity-hollow-left)
;;   (telephone-line-primary-right-separator 'telephone-line-flat)
;;   (telephone-line-secondary-right-separator 'telephone-line-identity-hollow-right)
;;   (telephone-line-faces
;;    '((evil . telephone-line-modal-face)
;;      (modal . telephone-line-modal-face)
;;      (ryo . telephone-line-ryo-modal-face)
;;      (accent . (telephone-line-accent-active . telephone-line-accent-inactive))
;;      (nil . (mode-line . mode-line-inactive))
;;      (modified . my/modified-face)
;;      (critical . (nano-critical-i . nano-subtle))
;;      (faded . (nano-faded-i . nano-subtle))))
;;   (telephone-line-lhs
;;    '((modified . (my/status-segment))
;;      (faded . (telephone-line-process-segment
;; 	      telephone-line-project-segment
;; 	      telephone-line-vc-segment
;; 	      my/buffer-segment))))
;;   (telephone-line-rhs
;;    '((faded . (telephone-line-flymake-segment
;; 	       telephone-line-misc-info-segment
;; 	       telephone-line-major-mode-segment))
;;      (modified . (telephone-line-airline-position-segment))))
;;   :init
;;   (telephone-line-defsegment* my/status-segment ()
;;     `(""
;;       mode-line-modified))
;;   (telephone-line-defsegment* my/buffer-segment ()
;;     `(""
;;       mode-line-client
;;       ,(telephone-line-raw mode-line-buffer-identification t)))
;;   (defun my/modified-face (active)
;;     (cond ((not active)
;; 	   (if (buffer-modified-p)
;; 	       'nano-popout-i
;; 	     'nano-subtle))
;; 	  ((buffer-modified-p)
;; 	   'nano-critical-i)
;; 	  (t 'nano-subtle)))
;;   (telephone-line-mode 1))

(custom-theme-set-faces 'user
			'(nano-vertico-prompt-face
			  ((t :inherit nano-critical-i))))

(setq tab-bar-show 1
      tab-bar-close-button-show nil
      tab-bar-new-tab-choice "*scratch*")

(add-hook 'text-mode-hook #'visual-line-mode)
;; (global-visual-wrap-prefix-mode 1)     ; TODO: do I want this or not?

(modify-all-frames-parameters
 '((internal-border-width . 24)
   (vertical-scroll-bars . nil)
   (right-divider-width . 12)
   (bottom-divider-width . 6)
   (tool-bar-lines . 0)))

(provide 'my-look)
;;; my-look.el ends here
