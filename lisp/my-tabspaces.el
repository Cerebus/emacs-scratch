;;; tabspaces configuration.

;;; Package management
(unless (package-installed-p 'tabspaces)
  (package-install 'tabspaces))

(require 'tabspaces)

; (customize-set-variable 'tabspaces-use-filtered-buffers-as-default t)
; (customize-set-variable 'tabspaces-default-tab "Default")
(customize-set-variable 'tabspaces-remove-to-default t)
(customize-set-variable 'tabspaces-include-buffers '("*scratch*"))
(customize-set-variable 'tabspaces-session nil)
(customize-set-variable 'tabspaces-session-auto-restore nil)

(tabspaces-mode 1)

;; Filter Buffers for Consult-Buffer

(with-eval-after-load 'consult
  ;; hide full buffer list (still available with "b" prefix)
  (consult-customize consult--source-buffer :hidden t :default nil)
  ;; set consult-workspace buffer list
  (defvar consult--source-workspace
    (list :name     "Workspace Buffers"
          :narrow   ?w
          :history  'buffer-name-history
          :category 'buffer
          :state    #'consult--buffer-state
          :default  t
          :items    (lambda () (consult--buffer-query
				:predicate #'tabspaces--local-buffer-p
				:sort 'visibility
				:as #'buffer-name)))

    "Set workspace buffer list for consult-buffer.")
  (add-to-list 'consult-buffer-sources 'consult--source-workspace))

;;; END
(provide 'my-tabspaces)
