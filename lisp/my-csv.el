;;; csv-mode

(unless (package-installed-p 'csv-mode)
  (package-install 'csv-mode))

(autoload 'csv-mode "csv-mode" "A mode for editing CSV files" t)

(provide 'csv-mode)