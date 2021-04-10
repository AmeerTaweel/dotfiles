;-------------------------------------------------------------------------------
; # Emacs Configuration
;-------------------------------------------------------------------------------

;; Load settings
(load "~/.emacs.d/lisp/settings")

;; Load packages
(load "~/.emacs.d/lisp/packages")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("75b8719c741c6d7afa290e0bb394d809f0cc62045b93e1d66cd646907f8e6d43" default))
 '(package-selected-packages
   '(org-bullets forge evil-magit magit counsel-projectile projectile hydra general evil-collection evil doom-themes helpful counsel which-key use-package rainbow-delimiters ivy-rich)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package doom-themes)

;; Change the theme
(load-theme 'doom-gruvbox t)

(defun dw/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil))

(use-package org
			 :hook (org-mode . dw/org-mode-setup)
			 :config
			 (setq org-ellipsis " ▾"
				   org-hide-emphasis-markers t))

(use-package org-bullets
			 :after org
			 :hook (org-mode . org-bullets-mode)
			 :custom
			 (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Replace list hyphen with dot
(font-lock-add-keywords 'org-mode
						'(("^ *\\([-]\\) "
						   (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; Read more
;; https://emacs.stackexchange.com/questions/62981/error-invalid-face-org-level-1
(with-eval-after-load 'org-faces (dolist (face '((org-level-1 . 1.2)
												 (org-level-2 . 1.1)
												 (org-level-3 . 1.05)
												 (org-level-4 . 1.0)
												 (org-level-5 . 1.1)
												 (org-level-6 . 1.1)
												 (org-level-7 . 1.1)
												 (org-level-8 . 1.1)))
								   (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face))))

;; Make sure org-indent face is available
(require 'org-indent)

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

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
