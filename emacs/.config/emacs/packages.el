;;;; Packages

;;; Imports

(require 'use-package)
(require 'load-relative)

;;; Rainbow Delimiters

(use-package rainbow-delimiters
  :diminish
  :hook (prog-mode . rainbow-delimiters-mode))

;;; Module End

(provide-me)
