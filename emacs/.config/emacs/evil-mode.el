;;;; Evil Mode
;;;; Based on the evil module of crafted-emacs

;;; Imports

(require 'use-package)
(require 'load-relative)

(require-relative "keys")

;;; Evil Mode

(use-package evil
  :diminish
  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-respect-visual-line-mode t)
  (evil-undo-system 'undo-redo)
  (evil-want-Y-yank-to-eol t)
  ;; C-i jumps forward in jump list like vim
  (evil-want-C-i-jump t)
  ;; Undo actions in several steps
  (evil-want-fine-undo t)
  ;; C-u scrolls up like vim
  (evil-want-C-u-scroll t)
  ;; C-d scrolls down like vim
  (evil-want-C-d-scroll t)
  ;; C-h is backspace in insert mode
  (evil-want-C-h-delete t)
  :config
  (evil-mode 1)
  ;; setup general for evil mode
  (general-evil-setup)
  ;; set leader key
  (evil-set-leader 'normal (kbd <prefix-evil>))
  ;; Use visual line motion
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  ;; Search more like vim
  (evil-select-search-module 'evil-search-module 'evil-search))

;;; Evil Collection
;;; Evil bindings for Emacs parts that Evil does not cover properly by default

(use-package evil-collection
  :diminish
  :diminish evil-collection-unimpaired-mode
  :after evil
  :config
  (evil-collection-init))

;;; Evil Commentary
;;; Port of vim-commentary plugin for Vim

(use-package evil-commentary
  :diminish
  :after evil
  :config
  (evil-commentary-mode 1))

;;; Module End

(provide-me)
