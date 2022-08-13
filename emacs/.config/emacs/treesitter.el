;;;; Tree Sitter

;;; Imports

(require 'use-package)
(require 'load-relative)

;;; Enable Tree Sitter

(use-package tree-sitter)

(use-package tree-sitter-langs
  :diminish
  :diminish tree-sitter-mode
  :after tree-sitter
  :config (global-tree-sitter-mode))

;;; Module End

(provide-me)
