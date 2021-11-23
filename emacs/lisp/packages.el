;-------------------------------------------------------------------------------
; # Emacs Packages
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
; ## Initialize
;-------------------------------------------------------------------------------

(require 'package)

;; Initialize package sources
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

;; Load package archives if they are not loaded
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

;; Ensure that all packages are installed
(setq use-package-always-ensure t)

;; Do not load packages on startup unless explicitly stated
(setq use-package-always-defer t)

;; Diminishes minor modes (hides them from status bar)
(use-package diminish)

;; Enable to debug packages
;; (setq use-package-verbose t)

;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
; ## Auto Package Updates
;-------------------------------------------------------------------------------

(use-package auto-package-update
  :demand t
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t))

;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
; ## Themes
;-------------------------------------------------------------------------------

;; The theme will be loaded at startup if it is the current theme
(use-package doom-themes)

;-------------------------------------------------------------------------------

;; General: More convenient key definitions in Emacs
(use-package general
  :demand t
  :config (general-evil-setup t))

;; Which Key: Display available keybindings in pop up
(use-package which-key
  :demand t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5)
  (setq which-key-idle-secondary-delay 0.001))

;; Rainbow Delimiters: Highlight delimiters according to their depth
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;-------------------------------------------------------------------------------
; ## Completion Front-End
;-------------------------------------------------------------------------------

;; Ivy: a generic completion front-end for Emacs
(use-package ivy
  :demand t
  :diminish
  :bind (:map ivy-minibuffer-map
	      ("TAB" . ivy-alt-done)
	      ("C-j" . ivy-next-line)
	      ("C-k" . ivy-previous-line)
	      :map ivy-switch-buffer-map
	      ("TAB" . ivy-done)
	      ("C-j" . ivy-next-line)
	      ("C-k" . ivy-previous-line)
	      ("C-d" . ivy-switch-buffer-kill))
  :init
  (ivy-mode 1))

;; Choose regex builder of ivy
(push '(t . ivy--regex-ignore-order) ivy-re-builders-alist)

;; Counsel: A collection of Ivy-enhanced versions of common Emacs commands.
(use-package counsel
  :demand t
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-switch-buffer)
	 ("C-x C-f" . counsel-find-file))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Remove the ^ from search prompts

;; Ivy-Rich: A more friendly interface for ivy.
(use-package ivy-rich
  :demand t
  :init (ivy-rich-mode 1))

;-------------------------------------------------------------------------------

;; Helpful: A better Emacs *help* buffer.
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(defun evil-hook ()
  (dolist (mode '(eshell-mode
		  shell-mode
		  term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (evil-mode)
  :hook (evil-mode . evil-hook)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package hydra)

(load "~/.emacs.d/lisp/p-hydra-config")

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :init
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config
  (counsel-projectile-mode 1))

(use-package magit
  :commands (magit-status magit-get-current-branch))

(use-package org)

(custom-set-variables
 '(org-directory "~/Documents/org-files")
 '(org-agenda-files (list org-directory))
 '(org-agenda-window-setup 'other-tab))

(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)

;; https://github.com/alphapapa/org-ql
;; Configure custom agenda views
(setq org-agenda-custom-commands
  '(("d" "Dashboard"
     ((agenda "Agenda")
      (todo "TODO"
        ((org-agenda-overriding-header "Todo Tasks")))
      (todo "DONE"
        ((org-agenda-overriding-header "Done Tasks")))))

    ("u" "University Tasks" tags-todo "+university")))

(setq org-refile-targets
      '(("archive.org" :level . 2)))

;; Save Org buffers after refiling!
(advice-add 'org-refile :after 'org-save-all-org-buffers)

;; https://github.com/akhramov/org-wild-notifier.el
;; https://github.com/progfolio/doct
(setq org-capture-templates
      `(("t" "Tasks")
	("tt" "Task" entry (file+olp "tasks.org" "Inbox")
	 "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

	("m" "Metrics")
	("mw" "Weight" table-line (file+headline "metrics.org" "Weight")
	 "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))


(define-key global-map (kbd "C-c j")
  (lambda () (interactive) (org-capture nil "j")))


(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 60)

(use-package dired
  :ensure nil
  :config
  (setq dired-listing-switches "-agho --group-directories-first")
  (with-eval-after-load 'evil-collection
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file)))

(use-package whitespace
  :demand t
  :init (global-whitespace-mode))

;; LSP Mode
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :hook (lsp-mode . efs/lsp-mode-setup)
  :config
  (lsp-enable-which-key-integration t))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))

(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-ivy)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))
