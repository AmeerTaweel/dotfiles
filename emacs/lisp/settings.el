;;-------------------------------------------------------------------------------
;; # Emacs Settings
;;-------------------------------------------------------------------------------

;;-------------------------------------------------------------------------------
;; ## Startup
;;-------------------------------------------------------------------------------

(defun settings/display-startup-time ()
  "Displays startup time and number of garbage collections done by Emacs"
  (message "Emacs loaded in %s with %d garbage collections."
		   (format "%.2f seconds"
				   (float-time
					 (time-subtract after-init-time before-init-time)))
		   gcs-done))

;; See startup time and number of garbage collections on startup
(add-hook 'emacs-startup-hook #'settings/display-startup-time)

;;-------------------------------------------------------------------------------

;;-------------------------------------------------------------------------------
;; ## Interface
;;-------------------------------------------------------------------------------

;; Turn off splash screen
;; The scratch buffer will show on startup
(setq inhibit-startup-message t)

;; Disable scroll-bar
(scroll-bar-mode -1)
;; Disable the tool-bar
(tool-bar-mode -1)
;; Disable tooltips
(tooltip-mode -1)
;; Disable menu-bar
(menu-bar-mode -1)

;; Enable the visible bell
(setq visible-bell t)

;; Disable line wrapping
(setq-default truncate-lines t)

;;-------------------------------------------------------------------------------

;;-------------------------------------------------------------------------------
;; ## Line Numbers
;;-------------------------------------------------------------------------------

;; Show column number in status line
(column-number-mode)
;; Show line numbers
(global-display-line-numbers-mode t)
;; Enable relative line numbers
(setq display-line-numbers-type 'relative)

;; Disable line numbers for some modes
(dolist (mode '(term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;-------------------------------------------------------------------------------

;;-------------------------------------------------------------------------------
;; ## Backup
;;-------------------------------------------------------------------------------

;; Change the directory where emacs saves backup files
(setq backup-directory-alist `(("." . "/tmp/emacs")))

;; Change backup strategy to not ruin symlinks
(setq backup-by-copying-when-linked t)

;;-------------------------------------------------------------------------------

;;-------------------------------------------------------------------------------
;; ## Terminal
;;-------------------------------------------------------------------------------

;; Turn on Unicode support in term and shell modes
(dolist (mode '(term-exec-hook
				 shell-mode-hook))
  (add-hook mode (lambda () (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))))

;;-------------------------------------------------------------------------------

;;-------------------------------------------------------------------------------
;; ## Others
;;-------------------------------------------------------------------------------

;; Automatically reload files was modified by external program
(global-auto-revert-mode 1)
(diminish auto-revert-mode)

;; Enable spell checking
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
;;-------------------------------------------------------------------------------
