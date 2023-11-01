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

;; Autoload magic doesn't seem to work here, wtf?
(autoload #'mu4e-icalendar-setup "mu4e-icalendar.el" "iCalendar integration")

(with-eval-after-load "mu4e"
  (substitute-key-definition 'mu4e-quit 'my/mu4e-quit mu4e-main-mode-map)
  (mailcap-parse-mimetypes (concat user-emacs-directory "mime.types") t)
  (epa-file-enable)
  (mu4e-icalendar-setup)
  (gnus-icalendar-org-setup)
  ;; Customize the trash mark to also mark as read.
  (setf (alist-get 'trash mu4e-marks)
        (list :char '("d" . "▼")
              :prompt "dtrash"
              :dyn-target (lambda (target msg)
                            (mu4e-get-trash-folder msg))
              :action (lambda (docid msg target)
                        (mu4e--server-move docid (mu4e--mark-check-target target) "+S-F-u-N"))))
  (setq mml-smime-use 'epg)
  (setf (alist-get 'refile mu4e-marks)
	(list :char '("r" . "▶")
	      :prompt "refile"
	      :dyn-target (lambda (target msg)
			    (mu4e-get-refile-folder msg))
	      :action (lambda (docid msg target)
			(mu4e--server-move docid
					   (mu4e--mark-check-target target)
					   "-F-N"))
	      ))
  (defun my-mu4e-unflag-and-file ()
    "Unflag and refile a message."
    (interactive)
    (mu4e-headers-mark-and-next 'unflag-refile))
  (define-key mu4e-headers-mode-map (kbd "_") 'my-mu4e-unflag-and-file)
  )

(defun my-message-cited-region ()
  "Target a region of contiguous cited lines starting with the line at point."
  (if (derived-mode-p 'message-mode)
      (save-excursion
	(let* ((start (progn (move-beginning-of-line nil) (point)))
	       (end (progn (while (progn (forward-line 1)
					 (looking-at message-yank-cited-prefix)))
			   (move-end-of-line nil) (point))))
	  `(region ,(buffer-substring start end) . (,start . ,end))))))

(with-eval-after-load 'embark
  (add-to-list 'embark-target-finders 'my-message-cited-region))

(provide 'my-email)
