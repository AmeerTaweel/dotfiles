;;;; Evil Mode
;;;; Based on the evil module of crafted-emacs

;;; Imports

(require 'use-package)
(require 'load-relative)

;;; Evil Mode

(use-package evil
  :diminish
  :init
  (customize-set-variable 'evil-want-integration t)
  (customize-set-variable 'evil-want-keybinding nil)
  (customize-set-variable 'evil-respect-visual-line-mode t)
  (customize-set-variable 'evil-undo-system 'undo-redo)
  (customize-set-variable 'evil-want-Y-yank-to-eol t)
  ;; C-i jumps forward in jump list like vim
  (customize-set-variable 'evil-want-C-i-jump t)
  ;; Undo actions in several steps
  (customize-set-variable 'evil-want-fine-undo t)
  ;; C-u scrolls up like vim
  (customize-set-variable 'evil-want-C-u-scroll t)
  ;; C-d scrolls down like vim
  (customize-set-variable 'evil-want-C-d-scroll t)
  :config
  (evil-mode 1)
  ;; Use visual line motion
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  ;; Search more like vim
  (evil-select-search-module 'evil-search-module 'evil-search)
  ;; C-h is backspace in insert mode
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join))

;;; Evil Collection
;;; Evil bindings for Emacs parts that Evil does not cover properly by default

(use-package evil-collection
  :diminish
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
