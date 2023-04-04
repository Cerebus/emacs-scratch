(customize-set-variable 'org-modules '(ol-doi ol-w3m ol-bibtex ol-gnus ol-info ol-irc ol-mhe ol-rmail ol-eww org-tempo))

(add-hook 'org-mode-hook
	  (lambda ()
	    (setq-local electric-pair-inhibit-predicate
			`(lambda (c)
			   (if (char-equal c ?<)
			       t ,electric-pair-inhibit-predicate c)))))

(provide 'my-org)
