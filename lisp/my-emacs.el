;;; my-emacs.el --- Core emacs changes             -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Timothy Miller

;; Author: Timothy Miller <cerebus2@gmail.com>
;; Keywords: local, convenience

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

;; Adding completing read and other common behaviors.

;;; Code:
(use-package savehist
  :init
  (savehist-mode 1))

(use-package vertico
  :ensure
  :custom
  (vertico-cycle t)
  :bind
  (:map vertico-map
	("DEL" . vertico-directory-delete-char)
	("M-DEL" . vertico-directory-delete-word))
  :init
  (vertico-mode 1)
  (vertico-mouse-mode 1))

(use-package orderless
  :ensure
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :ensure
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy
			   marginalia-annotators-light
			   nil))
  :init
  (marginalia-mode 1))

(use-package emacs
  :custom
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(use-package consult
  :ensure t
  :bind
  (("C-c M-x" . consult-mode-command)
   ("C-c h" . consult-history)
   ("C-c k" . consult-kmacro)
   ("C-c m" . consult-man)
   ("C-c i" . consult-info)
   ([remap Info-search] . consult-info)
   ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
   ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
   ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
   ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
   ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
   ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
   ("M-#" . consult-register-load)
   ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
   ("C-M-#" . consult-register)
   ("M-y" . consult-yank-pop)                ;; orig. yank-pop
   ("M-g e" . consult-compile-error)
   ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
   ("M-g g" . consult-goto-line)             ;; orig. goto-line
   ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
   ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)
   ("M-s d" . consult-find)
   ("M-s D" . consult-locate)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("C-x p g" . consult-ripgrep)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines)
   ("M-s e" . consult-isearch-history)
   :map isearch-mode-map
   ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
   ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
   ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
   ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
   :map minibuffer-local-map
   ("M-s" . consult-history)                 ;; orig. next-matching-history-element
   ("M-r" . consult-history)                ;; orig. previous-matching-history-element
   :map global-map
   ("C-s" . consult-line)
   ("C-r" . consult-line))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<") ;; "C-+"
)

(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))

(use-package consult-eglot
  :vc (:url "https://github.com/mohkale/consult-eglot")
  :after (eglot consult)
  :commands (consult-eglot-symbols consult-eglot-embark-mode)
  :bind
  ([remap xref-find-apropos] . consult-eglot-symbols)
  :config
  (require 'consult-eglot-embark)
  (consult-eglot-embark-mode))

(use-package consult-flyspell
  :vc (:url "https://github.com/emacsmirror/consult-flyspell")
  :bind
  (:map goto-map
	("s" . consult-flyspell)))

(use-package embark
  :ensure t
  :after (consult)
  :bind
  (([remap desribe-bindings] . embark-bindings)
   ("C-." . embark-act)
   ("C-;" . embark-dwim)
   :map embark-expression-map
   ("d" . delete-pair))
  :hook
  ((embark-collect-mode . consult-preview-at-point-mode))
  :init
  (setq prefix-help-command 'embark-prefix-help-command)
  :config
  (add-to-list 'embark-pre-action-hooks '(delete-pair embark--beginning-of-target)))

(use-package embark-consult
  :ensure t
  :after (embark consult))

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  :init
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1)
  (eldoc-add-command 'corfu-insert)
  :bind
  (:map corfu-map
	("M-p" . corfu-popupinfo-scroll-down)
	("M-n" . corfu-popupinfo-scroll-up)
	("M-d" . corfu-popupinfo-toggle)))

(use-package corfu-terminal
  :unless (display-graphic-p)
  :ensure t
  :defer t
  :init
  (corfu-terminal-mode +1))


(provide 'my-emacs)
;;; my-completion.el ends here
