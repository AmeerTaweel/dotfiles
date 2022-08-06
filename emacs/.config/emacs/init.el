;;;; Emacs Configuration

;;; Setup

;; do not install packages from within emacs
(setq use-package-always-ensure nil)

(require 'use-package)

(use-package load-relative)

;;; Imports

(require-relative "settings")
(require-relative "theme")


(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/knowledge-base")
  ;; :general
  ;; (general-define-key
  ;;  :states 'normal
  ;;  :prefix (concat <leader> "or")
  ;;  "f" '(org-roam-node-find :which-key "org-roam find node")
  ;;  "b" '(org-roam-buffer-toggle :which-key "org-roam display buffer")
  ;;  "i" '(org-roam-node-insert :which-key "org-roam insert node"))
  :config
  (make-directory org-roam-directory t) ; ensure directory exists
  (org-roam-setup))

  ;; (:keymaps 'pdf-view-mode-map
	    ;; "TAB" 'org-cycle))
  ;; (general-unbind '(normal visual) 'pdf-view-mode-map
    ;; "C-h"
    ;; "C-j"
    ;; "C-k"
    ;; "C-l"))
  ;; (add-hook 'pdf-view-mode-hook
	    ;; (lambda ()
	      ;; ))

;; TODO: no cluttering every directory
