(require 'my-tree-sitter)

(my-tree-sitter-init
 'json
 'json-mode
 'json-ts-mode
 '("https://github.com/tree-sitter/tree-sitter-json")
 )

(provide 'my-json)
