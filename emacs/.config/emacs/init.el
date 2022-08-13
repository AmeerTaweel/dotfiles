;;;; Emacs Configuration

;;; Imports

(require 'load-relative)

;; General Modules

(require-relative "settings")
(require-relative "theme")
(require-relative "packages")
(require-relative "completion")
(require-relative "evil-mode")
(require-relative "org-mode")
(require-relative "treesitter")
(require-relative "terminal")

;; Language-Specific Modules

(require-relative "lisp")
(require-relative "nix")
