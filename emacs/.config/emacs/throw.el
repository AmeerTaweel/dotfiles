;;; Evil Mode



;; Disable line numbers for some buffer types
;; (let ((modes '(shell-mode-hook
;;	       term-mode-hook
;;	       eshell-mode-hook)))
;;  (dolist (mode modes)
;;    (add-hook mode (lambda () (display-line-numbers-mode 0)))))

; close prompts with escaep
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; (use-package evil
;;  :init
;;  ; evil-want-integration is set to t by default
;;  (setq evil-want-integration t)
;;  (setq evil-want-keybinding nil)
;;  (setq evil-want-C-u-scroll t)
;;  :config
;;  (evil-mode 1)
;;  ;; Use visual line motion
;;  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
;;  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
;;  :diminish)

;; (use-package evil-collection
;;  :after evil
;;  :config
;;  (setq evil-collection-mode-list nil)
;;  (evil-collection-init)
;;  :diminish)

;;; Mode Line

;; (use-package telephone-line
;;  :config
;;  (telephone-line-mode 1)
;;  :diminish)

;; Keybindings

;; (defconst <leader> ",")

;; (use-package general
;;  :after evil
;;  :config
;;  (general-evil-setup)
;;  ;; Split Windows
;;  (general-define-key
;;   :states 'normal
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

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
	 ("C-l" . ivy-done)
	 ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
	 ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start search with ^
(use-package ivy-rich
  :init (ivy-rich-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

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
