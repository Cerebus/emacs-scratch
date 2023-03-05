(require 'treesit)

(defun my-tree-sitter-init (lang old-mode new-mode source)
  "Ensure a tree-sitter parser for LANG is installed from SOURCE and REVISION,
overriding the original major-mode OLD-MODE with NEW-MODE.
SOURCE is in the format for `treesit-language-source-alist'."
  (add-to-list 'major-mode-remap-alist (cons old-mode new-mode))
  (add-to-list 'treesit-language-source-alist (cons lang source))
  (unless (treesit-language-available-p lang)
    (treesit-install-language-grammar lang)))
  
(provide 'my-tree-sitter)
