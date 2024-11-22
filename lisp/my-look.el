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
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Deep changes to Emacs look and feel, beyond just theming.

;;; Code:
(setq-default mode-line-format nil)

(use-package nano-theme
  :init
  (load-theme 'nano t)
  :custom
  (nano-fonts-use t)
  :custom-face
  (nano-mono ((t (:family "RobotoMono Nerd Font Mono"))))
  (nano-sans ((t (:family "FiraCode Nerd Font Propo"))))
  (nano-italic ((t (:family "RobotoMono Nerd Font Mono"))))
  (tab-bar-tab ((t (:inherit nano-strong))))
  (tab-bar-tab-inactive ((t (:inherit nano-subtle))))
  :vc (:url "https://github.com/rougier/nano-theme"))

(use-package nano-vertico
  :vc (:url "https://github.com/rougier/nano-vertico")
  :custom
  (nano-vertico-prompt nil)
  :config
  (nano-vertico-mode 1))

(use-package telephone-line
  :vc (:url "https://github.com/dbordak/telephone-line" :rev :newest)
  :commands
  (telephone-line-defsegment*)
  :custom
  (telephone-line-target 'header-line)
  (telephone-line-height (* (font-get (face-attribute 'default :font) :size) 2))
  (telephone-line-primary-left-separator 'telephone-line-flat)
  (telephone-line-secondary-left-separator 'telephone-line-identity-hollow-left)
  (telephone-line-primary-right-separator 'telephone-line-flat)
  (telephone-line-secondary-right-separator 'telephone-line-identity-hollow-right)
  (telephone-line-faces
   '((evil . telephone-line-modal-face)
     (modal . telephone-line-modal-face)
     (ryo . telephone-line-ryo-modal-face)
     (accent . (telephone-line-accent-active . telephone-line-accent-inactive))
     (nil . (mode-line . mode-line-inactive))
     (modified . my/modified-face)
     (critical . (nano-critical-i . nano-subtle))
     (faded . (nano-faded-i . nano-subtle))))
  (telephone-line-lhs
   '((modified . (my/status-segment))
     (faded . (telephone-line-process-segment
	      telephone-line-project-segment
	      telephone-line-vc-segment
	      my/buffer-segment))))
  (telephone-line-rhs
   '((faded . (telephone-line-flymake-segment
	       telephone-line-misc-info-segment
	       telephone-line-major-mode-segment
	       telephone-line-airline-position-segment))))
  :init
  (telephone-line-defsegment* my/status-segment ()
    `(""
      mode-line-modified))
  (telephone-line-defsegment* my/buffer-segment ()
    `(""
      mode-line-client
      ,(telephone-line-raw mode-line-buffer-identification t)))
  (defun my/modified-face (active)
    (cond ((not active) 'nano-subtle)
	  ((buffer-modified-p)
	   'nano-critical-i)
	  (t 'nano-subtle)))
  (telephone-line-mode 1))

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
