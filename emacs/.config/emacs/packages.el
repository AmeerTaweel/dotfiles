;;;; Packages

;;; Imports

(require 'use-package)
(require 'load-relative)

(require-relative "keys")

;;; Diminish

(use-package emacs
  :diminish eldoc-mode)

;;; Keybindings

(use-package general
  :diminish
  :config
  ;; Movement
  (general-define-key
   "C-h" '(windmove-left :which-key "navigate window left")
   "C-j" '(windmove-down :which-key "navigate window down")
   "C-k" '(windmove-up :which-key "navigate window up")
   "C-l" '(windmove-right :which-key "navigate window right"))
  ;; Alternative help keybindings
  ;; Because the C-h prefix is taken for movement
  (general-define-key
   :prefix (custom/keys-join <prefix-emacs> "h")
   "b" 'describe-bindings
   "c" 'describe-command
   "f" 'describe-function
   "k" 'describe-key
   "s" 'describe-symbol
   "v" 'describe-variable)
  ;; Split windows keybindings
  (general-define-key
   :prefix <prefix-emacs>
   "/" '(split-window-horizontally :which-key "left-right window split")
   "-" '(split-window-vertically :which-key "top-bottom window split"))
  )

;;; Rainbow Delimiters

(use-package rainbow-delimiters
  :diminish
  :hook
  (prog-mode . rainbow-delimiters-mode))

;;; Which Key

(use-package which-key
  :diminish
  :init
  (which-key-mode 1)
  :custom
  (which-key-idle-delay 0.5))

;;; Helpful
;;; Better *help* buffer

(use-package helpful
  :diminish
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-symbol] . helpful-symbol))

;;; TODO Highlight

(use-package hl-todo
  :custom
  (hl-todo-highlight-punctuation ":")
  (hl-todo-keyword-faces
   `(("TODO" warning bold)
     ("FIXME" error bold)
     ("HACK" font-lock-constant-face bold)
     ("REVIEW" font-lock-keyword-face bold)
     ("NOTE" success bold)
     ("DEPRECATED" font-lock-doc-face bold)))
  :hook
  ((prog-mode . hl-todo-mode)
   (yaml-mode . hl-todo-mode)))

;;; Module End

(provide-me)
