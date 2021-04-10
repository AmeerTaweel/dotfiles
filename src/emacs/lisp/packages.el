;-------------------------------------------------------------------------------
; # Emacs Plugin
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

;; Diminishes minor modes (hides them from status bar)
(use-package diminish)

(use-package doom-themes)

;; Better key bindings
(use-package general
  :config (general-evil-setup t))

(load "~/.emacs.d/lisp/p-general-config")

;; Key binding live list
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.25)
  (setq which-key-idle-secondary-delay 0.001))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package ivy
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

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-switch-buffer)
	 ("C-x C-f" . counsel-find-file))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Remove the ^ from search prompts

(use-package ivy-rich
  :init (ivy-rich-mode 1))

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

(use-package forge)

(use-package org)

(setq org-directory "~/Projects/Code/emacs-from-scratch/OrgFiles")
(setq org-agenda-files '("Tasks.org" "Birthdays.org" "Habits.org"))

;; If you only want to see the agenda for today
;; (setq org-agenda-span 'day)

(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
	(sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

;; Configure custom agenda views
(setq org-agenda-custom-commands
      '(("d" "Dashboard"
	 ((agenda "" ((org-deadline-warning-days 7)))
	  (todo "NEXT"
		((org-agenda-overriding-header "Next Tasks")))
	  (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

	("n" "Next Tasks"
	 ((todo "NEXT"
		((org-agenda-overriding-header "Next Tasks")))))


	("W" "Work Tasks" tags-todo "+work")

	;; Low-effort next actions
	("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
	 ((org-agenda-overriding-header "Low Effort Tasks")
	  (org-agenda-max-todos 20)
	  (org-agenda-files org-agenda-files)))

	("w" "Workflow Status"
	 ((todo "WAIT"
		((org-agenda-overriding-header "Waiting on External")
		 (org-agenda-files org-agenda-files)))
	  (todo "REVIEW"
		((org-agenda-overriding-header "In Review")
		 (org-agenda-files org-agenda-files)))
	  (todo "PLAN"
		((org-agenda-overriding-header "In Planning")
		 (org-agenda-todo-list-sublevels nil)
		 (org-agenda-files org-agenda-files)))
	  (todo "BACKLOG"
		((org-agenda-overriding-header "Project Backlog")
		 (org-agenda-todo-list-sublevels nil)
		 (org-agenda-files org-agenda-files)))
	  (todo "READY"
		((org-agenda-overriding-header "Ready for Work")
		 (org-agenda-files org-agenda-files)))
	  (todo "ACTIVE"
		((org-agenda-overriding-header "Active Projects")
		 (org-agenda-files org-agenda-files)))
	  (todo "COMPLETED"
		((org-agenda-overriding-header "Completed Projects")
		 (org-agenda-files org-agenda-files)))
	  (todo "CANC"
		((org-agenda-overriding-header "Cancelled Projects")
		 (org-agenda-files org-agenda-files)))))))

(setq org-refile-targets
      '(("Archive.org" :maxlevel . 1)))

;; Save Org buffers after refiling!
(advice-add 'org-refile :after 'org-save-all-org-buffers)


(defun dw/read-file-as-string (path)
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(setq org-capture-templates
      `(("t" "Tasks / Projects")
	("tt" "Task" entry (file+olp "~/Projects/Code/emacs-from-scratch/OrgFiles/Tasks.org" "Inbox")
	 "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
	("ts" "Clocked Entry Subtask" entry (clock)
	 "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

	("j" "Journal Entries")
	("jj" "Journal" entry
	 (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
	 "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
	 ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
	 :clock-in :clock-resume
	 :empty-lines 1)
	("jm" "Meeting" entry
	 (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
	 "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
	 :clock-in :clock-resume
	 :empty-lines 1)

	;; ("w" "Workflows")
	;; ("we" "Checking Email" entry (file+olp+datetree ,(dw/get-todays-journal-file-name))
	;;   "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

	("m" "Metrics Capture")
	("mw" "Weight" table-line (file+headline "~/Projects/Code/emacs-from-scratch/OrgFiles/Metrics.org" "Weight")
	 "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))


(define-key global-map (kbd "C-c j")
  (lambda () (interactive) (org-capture nil "j")))


(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 60)
