;;; mu4e for great justice!

;; Fun complications; mu4e distributes with mu, so installation varies a bit.
(when (eq system-type 'darwin)
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu")) ; brew links it here

(autoload 'mu4e "mu4e" "Mu Email client" t)


(require 'my-visual)
(add-hook 'mu4e-view-mode-hook #'visual-fill-column-mode)
(add-hook 'mu4e-view-mode-hook (lambda ()
				 (setq visual-fill-column-center-text t)
				 (adaptive-wrap-prefix-mode)))
(add-hook 'mu4e-compose-mode-hook #'visual-fill-column-mode)
(add-hook 'mu4e-compose-mode-hook (lambda ()
				    (setq visual-fill-column-center-text t)
				    (auto-fill-mode -1)
				    (adaptive-wrap-prefix-mode)))

(global-set-key (kbd "C-c o m") #'mu4e)

(setopt mu4e-completing-read-function completing-read-function)

(with-eval-after-load "mu4e"
  (add-to-list 'mailcap-mime-extensions '(".docx" . "application/vnd.openxmlformats-officedocument.wordprocessingml.document"))
  (add-to-list 'mailcap-mime-extensions '(".pptx" . "application/vnd.openxmlformats-officedocument.presentationml.presentation"))
  (add-to-list 'mailcap-mime-extensions '(".xlsx" . "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"))
  (setq epg-pinentry-mode nil)
  (epa-file-enable)
  (require 'smtpmail)
  (with-eval-after-load "mm-decode"
    (add-to-list 'mm-discouraged-alternatives "text/html")
    (add-to-list 'mm-discouraged-alternatives "text/richtext"))
  (require 'mu4e-icalendar)
  (mu4e-icalendar-setup)
  (gnus-icalendar-org-setup)
  (add-to-list 'mu4e-bookmarks
               '(:name "Flagged"
                       :query "flag:flagged"
                       :key ?f
                       )
               )
  (add-to-list 'mu4e-bookmarks
             '(:name "Unhandled total"
                     :query "maildir:/INBOX"
                     :key ?I
                     )
             )
  (add-to-list 'mu4e-bookmarks
               '(:name "Unhandled this week"
                       :query "maildir:/INBOX and date:1w..today"
                       :key ?W
                       )
               )
  (add-to-list 'mu4e-bookmarks
               '(:name "Unhandled today"
                       :query "maildir:/INBOX AND date:today"
                       :key ?T
                       )
               )
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
