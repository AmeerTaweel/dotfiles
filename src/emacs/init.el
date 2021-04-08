; Emacs Configuration

;; Turn off the welcome screen
;; (setq inhibit-startup-message t)

;; Disable scroll-bar
(scroll-bar-mode -1)
;; Disable the tool-bar
;; (tool-bar-mode -1)
;; Disable tooltips
;; (tooltip-mode -1)
;; Disable menu-bar
;; (menu-bar-mode -1)

;; Setup the visible bell
(setq visible-bell t)

;; Change font and font size
;; (set-face-attributes 'default nil :font "Fira Code Retina" :height 280)

;; Change the theme
(load-theme 'misterioso)

;; Package Management
(require 'package)

;;; Initialize package sources
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

;;; Load package archives if they are not loaded
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)

;;; Ensure that all packages are installed
(setq use-package-always-ensure t)
