;; do not install packages from within emacs
(setq use-package-always-ensure nil)

(require 'use-package)

;; Disable line numbers for some buffer types
;; (let ((modes '(shell-mode-hook
;;	       term-mode-hook
;;	       eshell-mode-hook)))
;;  (dolist (mode modes)
;;    (add-hook mode (lambda () (display-line-numbers-mode 0)))))

; close prompts with escaep
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)



;;; Mode Line

;; (use-package telephone-line
;;  :config
;;  (telephone-line-mode 1)
;;  :diminish)

;; Keybindings

;; (defconst <leader> ",")

(use-package general
  :after evil
  :config
  (general-evil-setup)
  ;; Split Windows
  (general-define-key
   :states 'normal
   :prefix <leader>
   "/" '(evil-window-vsplit :which-key "split window vertically")
   "-" '(evil-window-split :which-key "split window horizontally"))
  ;; Buffers
  (general-define-key
   :states 'normal
   :prefix (concat <leader> "b")
   "k" '(kill-this-buffer :which-key "kill buffer")
   "r" '(revert-buffer :which-key "refresh buffer"))
  ;; Movement
  (general-define-key
   :states 'normal
   ;; :keymaps 'override
   "C-h" '(evil-window-left :which-key "navigate window left")
   "C-j" '(evil-window-down :which-key "navigate window down")
   "C-k" '(evil-window-up :which-key "navigate window up")
   "C-l" '(evil-window-right :which-key "navigate window right")))

(use-package which-key
  :init (which-key-mode)
  :diminish
  :config (setq which-key-idle-delay 0.5))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))


;; (require-relative "pdf")

(use-package hl-todo
  :config
  (setq hl-todo-highlight-punctuation ":"
	hl-todo-keyword-faces
	`(("TODO" warning bold)
	  ("FIXME" error bold)
	  ("HACK" font-lock-constant-face bold)
	  ("REVIEW" font-lock-keyword-face bold)
	  ("NOTE" success bold)
	  ("DEPRECATED" font-lock-doc-face bold)))
  :hook ((prog-mode . hl-todo-mode)
	 (yaml-mode . hl-todo-mode)))

;; Make sure some modes start in Emacs state
;; TODO: Split this out to other configuration modules?
;; (dolist (mode '(custom-mode
   ;;             eshell-mode
     ;;           term-mode))
 ;; (add-to-list 'evil-emacs-state-modes mode))
