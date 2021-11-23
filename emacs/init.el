;;-------------------------------------------------------------------------------
;; # Emacs Configuration
;;-------------------------------------------------------------------------------

;; Increase the garbage collector threshold to make startup faster
;; The default is 800 kilobytes
;; Current value is 50 megabytes (measured in bytes)
(setq gc-cons-threshold (* 50 1000 1000))

;;-------------------------------------------------------------------------------
;; ## Auto-Created Block - Do not edit by hand
;;-------------------------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("75b8719c741c6d7afa290e0bb394d809f0cc62045b93e1d66cd646907f8e6d43" default))
 '(org-agenda-files (list org-directory))
 '(org-agenda-window-setup 'other-tab)
 '(org-directory "~/Documents/org-files")
 '(package-selected-packages
   '(lsp-ivy lsp-ui company-box company typescript-mode lsp-mode which-key use-package rainbow-delimiters org-bullets ivy-rich hydra helpful general forge evil-collection eterm-256color doom-themes diminish counsel-projectile auto-package-update)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;-------------------------------------------------------------------------------

;; Load packages
(load "~/.emacs.d/lisp/packages")

;; Load settings
(load "~/.emacs.d/lisp/settings")

;; Load keybindings
(load "~/.emacs.d/lisp/keybindings")

;; Load theme
(load "~/.emacs.d/lisp/theme")

;; Run garbage collector more frequently by decreasing the threshold
;; Current value is 5 megabytes (measured in bytes)
(setq gc-cons-threshold (* 5 1000 1000))
