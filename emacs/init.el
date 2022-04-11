;; Emacs Reference Card
;; https://www.gnu.org/software/emacs/refcards/pdf/refcard.pdf

(setq inhibit-startup-message t
      visible-bell nil)

(tool-bar-mode -1)
(scroll-bar-mode -1)

;; TODO: Uncomment when I get more used to Emacs
;; (menu-bar-mode -1)

(global-display-line-numbers-mode 1)
(column-number-mode)
;; Disable line numbers for some buffer types
(dolist (mode '(shell-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
	  

;; Remember recent open files
;; Run M-x recentf-open-files to use
;; (recentf-mode 1)

;; Control history length
;; (setq history-length 25)
;; Save minibuffer history
;; (savehist-mode 1)

;; Remember the last cursor location in a file and restore on open
(save-place-mode 1)

(load-theme 'wombat t)

;; Use a different file for customization variables
;; (setq custom-file (locate-user-emacs-file "custom-vars.el"))
;; (load custom-file 'noerror 'nomessage)

;; Don't use UI popups
(setq use-dialog-box nil)

;; Refresh buffer on external change
(global-auto-revert-mode 1)

;; Revert dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Close prompts with escaep
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Do not install packages
;; Packages are installed by NixOS
(setq use-package-always-ensure nil)

(require 'use-package)

(use-package evil
  :init
  ; evil-want-integration is set to t by default
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  :diminish)

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
  :diminish)

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

(use-package doom-themes)
