(unless (package-installed-p 'ace-window)
  (package-install 'ace-window))

(global-set-key (kbd "C-x o") #'ace-window)

(customize-set-variable 'aw-scope 'frame)

(provide 'my-ace-window)
