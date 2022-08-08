;;;; General Settings

;;; Imports

(require 'load-relative)

; disable startup screen
(setq inhibit-startup-message t)
; disble visible bell
(setq visible-bell nil)
; disable toolbar
(tool-bar-mode -1)
; disable scrollbar
(scroll-bar-mode -1)

; enable line numbers
(global-display-line-numbers-mode 1)

; remember recent open files
; run M-x recentf-open-files to use
(recentf-mode 1)

(setq-default tab-width 4)

(setq use-short-answers t)

; save minibuffer history
(savehist-mode 1)

; remember last cursor location in a file and restore when open
(save-place-mode 1)

; use different file for customization variables
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

; disable ui popups
(setq use-dialog-box nil)

; refresh buffer on external change
(global-auto-revert-mode 1)
; revert dired and other buffers
(setq global-auto-revert-non-file-buffers t)

; show column number in status bar
(column-number-mode)

; enable fill-column-indicator
; also named color-column in vim
(setq fill-column 80)
(global-display-fill-column-indicator-mode 1)

; put all backup files in the system's "temp" directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
; put all auto-save files in the system's "temp" director
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(customize-set-variable 'scroll-step 1)
(customize-set-variable 'scroll-margin 3)

;; make prompt read-only in shell mode
(customize-set-variable 'comint-prompt-read-only t)

;;; Module End

(provide-me)
