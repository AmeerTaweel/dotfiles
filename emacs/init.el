(setq inhibit-startup-message t
      visible-bell nil)

(tool-bar-mode -1)
(scroll-bar-mode -1)

;; TODO: Uncomment when I get more used to Emacs
;; (menu-bar-mode -1)

(global-display-line-numbers-mode 1)

;; Remember recent open files
;; Run M-x recentf-open-files to use
;; (recentf-mode 1)

;; Control history length
;; (setq history-length 25)
;; Save minibuffer history
;; (savehist-mode 1)

;; Remember the last cursor location in a file and restore on open
(save-place-mode 1)

;; Use a different file for customization variables
;; (setq custom-file (locate-user-emacs-file "custom-vars.el"))
;; (load custom-file 'noerror 'nomessage)

;; Don't use UI popups
(setq use-dialog-box nil)

;; Refresh buffer on external change
(global-auto-revert-mode 1)

;; Revert dired and other buffers
(setq global-auto-revert-non-file-buffers t)
