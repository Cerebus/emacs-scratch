(unless (package-installed-p 'ace-window)
  (package-install 'ace-window))

(global-set-key (kbd "C-x o") #'ace-window)

(provide 'my-ace-window)
