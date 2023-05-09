;;; mu4e for great justice!

;; Fun complications; mu4e distributes with mu, so installation varies a bit.
(when (eq system-type 'darwin)
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu")) ; brew links it here

(autoload 'mu4e "mu4e" "Mu Email client" t)

(require 'my-visual)

(add-hook 'mu4e-main-mode-hook (lambda ()
				 (display-line-numbers-mode -1)))

(add-hook 'mu4e-view-mode-hook #'visual-fill-column-mode)

(add-hook 'mu4e-view-mode-hook (lambda ()
				 (setq visual-fill-column-center-text t)
				 (visual-line-mode 1)
				 (display-line-numbers-mode -1)
				 (adaptive-wrap-prefix-mode)))
(add-hook 'mu4e-compose-mode-hook #'visual-fill-column-mode)

(add-hook 'mu4e-compose-mode-hook (lambda ()
				    (setq visual-fill-column-center-text t)
				    (visual-line-mode 1)
				    (auto-fill-mode -1)
				    (adaptive-wrap-prefix-mode)))

;; (global-set-key (kbd "C-c o m") #'mu4e)
(global-set-key (kbd "C-c o m") (lambda ()
				  (interactive)
				  (tab-bar-switch-to-tab "*mu4e-main*")
				  (mu4e)))

(setopt mu4e-completing-read-function completing-read-function)

(defun my/mu4e-quit ()
  (interactive)
  (tab-close)
  (call-interactively 'mu4e-quit))

(with-eval-after-load "mu4e"
  (substitute-key-definition 'mu4e-quit 'my/mu4e-quit mu4e-main-mode-map)
  (mailcap-parse-mimetypes (concat user-emacs-directory "mime.types"))
  (setq epg-pinentry-mode nil)
  (epa-file-enable)
  (require 'smtpmail)
  (with-eval-after-load "mm-decode"
    (add-to-list 'mm-discouraged-alternatives "text/html")
    (add-to-list 'mm-discouraged-alternatives "text/richtext"))
  (require 'mu4e-icalendar)
  (mu4e-icalendar-setup)
  (gnus-icalendar-org-setup)

  (setf (alist-get 'trash mu4e-marks)
        (list :char '("d" . "▼")
              :prompt "dtrash"
              :dyn-target (lambda (target msg)
                            (mu4e-get-trash-folder msg))
              :action (lambda (docid msg target)
                        ;; Here's the main difference to the regular trash mark,
                        ;; no +T before -N so the message is not marked as
                        ;; IMAP-deleted:
                        (mu4e--server-move docid (mu4e--mark-check-target target) "-u-N")))) ; Mark trash as read
  (setq mu4e-update-interval 120
      mu4e-read-option-use-builtin t
      mml-default-encrypt-method "smime"
      mml-default-sign-method "smime"
      mml-secure-method "smime"
      mml-secure-smime-encrypt-to-self t
      mml-secure-smime-sign-with-sender t
      mm-verify-option 'always
      mm-smime-use 'epg
      mm-decrypt-option 'always
      gnus-buttonized-mime-types '("multipart/encrypted" "multipart/signed")
      smime-CA-directory (expand-file-name "~/.trust")
      mu4e-headers-fields '((:maildir . 8)
                            (:human-date . 12)
                            (:flags . 6)
                            (:from-or-to . 25)
                            (:subject))
      mu4e-headers-show-threads t
      mu4e-headers-sort-direction 'ascending
      mu4e-headers-include-related nil
      alert-default-style 'notifier
      mu4e-change-filenames-when-moving t
      message-kill-buffer-on-exit t
      message-sendmail-f-is-evil t
      ;; message-sendmail-extra-arguments '("--read-envelope-from")
      ;; message-send-mail-function #'message-send-mail-with-sendmail
      ;; mail-specify-envelope-from t
      ;; mail-envelope-from 'header
      ;; message-sendmail-envelope-from 'header
      ;; sendmail-program (executable-find "msmtp")
      send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      )
  )

(provide 'my-email)
