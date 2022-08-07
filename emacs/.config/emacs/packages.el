;;;; Packages

;;; Imports

(require 'use-package)
(require 'load-relative)

;;; Rainbow Delimiters

(use-package rainbow-delimiters
  :diminish
  :hook
  (prog-mode . rainbow-delimiters-mode))

;;; Which Key

(use-package which-key
  :diminish
  :init
  (which-key-mode 1)
  :config
  (customize-set-variable 'which-key-idle-delay 0.5))

;;; Module End

(provide-me)
