;;;; Packages

;;; Imports

(require 'use-package)
(require 'load-relative)

;;; Nix Mode

(use-package nix-mode
  :hook
  ((nix-mode . indent-tabs-mode)))

;;; Module End

(provide-me)
