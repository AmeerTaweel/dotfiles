;;;; Keys

;;; Imports

(require 'load-relative)

;;; Prefixes

(defconst <prefix-emacs> "C-a")
(defconst <prefix-evil> ",")

;;; Clear Prefix

(global-unset-key (kbd <prefix-emacs>))

;;; Utils

(defun custom/keys-join (&rest keys)
  "Join keys into one string with space as a delimiter."
  (mapconcat 'identity keys " "))

;;; Module End

(provide-me)
