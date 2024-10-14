;;; my-cmake.el ---                                  -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:
(my-tree-sitter-init
 'cmake
 'cmake-ts-mode
 'cmake-ts-mode
 '("https://github.com/uyha/tree-sitter-cmake"))

(add-to-list 'auto-mode-alist
             '("\\(?:CMakeLists\\.txt\\|\\.cmake\\)\\'" . cmake-ts-mode))

(add-hook 'cmake-ts-mode-hook #'eglot-ensure)

(provide 'my-cmake)
;;; my-cmake.el ends here
