;;; Imports

(require 'use-package)
(require 'load-relative)

(require-relative "keys")

;;; Org Roam
;;; Plain-text personal knowledge management system

(use-package org-roam
  :custom
  (org-roam-v2-ack t)
  (org-roam-directory "~/knowledge-base")
  :general
  (general-define-key
   :prefix (custom/keys-join <prefix-emacs> "o" "r")
   "f" 'org-roam-node-find
   "b" 'org-roam-buffer-toggle
   "i" 'org-roam-node-insert)
  :config
  (make-directory org-roam-directory t) ; ensure directory exists
  (org-roam-setup))

;;; Module End

(provide-me)
