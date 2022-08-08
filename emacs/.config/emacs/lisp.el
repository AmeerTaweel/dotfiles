;;;; Lisp Support
;;;; Based on the lisp module of crafted-emacs

;;; Imports

(require 'use-package)
(require 'load-relative)

;;; Common Lisp

;; SLY
;; Common Lisp IDE for Emacs
;; Fork of SLIME

(use-package sly
  :diminish
  :hook
  (lisp-mode . sly-editing-mode))

(use-package sly-asdf
  :diminish
  :after sly)

(use-package sly-repl-ansi-color
  :diminish
  :after sly)

;;; Scheme
;; Scheme IDE for Emacs

(use-package geiser
  :diminish)

(use-package geiser-guile
  :diminish
  :after geiser)

(use-package geiser-racket
  :diminish
  :after geiser)

;;; Module End

(provide-me)
