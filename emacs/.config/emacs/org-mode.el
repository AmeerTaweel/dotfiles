;;;; Org Mode

;;; Imports

(require 'use-package)
(require 'load-relative)

(require-relative "keys")

;;; Knowledge Base
;;; My vault of org files

(defconst custom/org-mode-knowledge-base "~/knowledge-base/")

;; Ensure knowledge base exists
(make-directory custom/org-mode-knowledge-base t)

;;; Org Roam
;;; Plain-text personal knowledge management system

(use-package org-roam
  :custom
  (org-roam-v2-ack t)
  (org-roam-directory custom/org-mode-knowledge-base)
  :general
  (general-define-key
   :prefix (custom/keys-join <prefix-emacs> "o" "r")
   "f" 'org-roam-node-find
   "b" 'org-roam-buffer-toggle
   "i" 'org-roam-node-insert)
  :config
  (org-roam-setup))

;;; Org Agenda

(use-package org
  :custom
  (org-agenda-files `(,custom/org-mode-knowledge-base))
  ;; Display the time a task was done in the agenda view
  (org-agenda-start-with-log-mode t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  ;; Add custom state workflows
  (org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d!)" "CANCELLED(c!)")))
  :general
  (general-define-key
   :prefix (custom/keys-join <prefix-emacs> "o" "a")
   "a" 'org-agenda
   "l" 'org-agenda-list))

;;; Capture Templates

(defconst custom/org-mode--todo-list "~/knowledge-base/20220811000520-todo_list.org")

(use-package doct
  :custom
  (org-capture-templates
   (doct '(("Task" :keys "t"
			:file custom/org-mode--todo-list
			:template ("* TODO %?" "%U" "%a")
			:headline "Captured"
			:prepend t
			:kill-buffer t))))
  :general
  (general-define-key
   :prefix (custom/keys-join <prefix-emacs> "o")
   "c" 'org-capture))

;;; Org Habit

(use-package org-habit
  :config
  (add-to-list 'org-modules 'org-habit))

;;; Org Timer

(use-package org
  :general
  (general-define-key
   :prefix (custom/keys-join <prefix-emacs> "o" "t")
   "s" 'org-timer-set-timer
   "t" 'org-timer-pause-or-continue
   "k" 'org-timer-stop))

;;; Module End

(provide-me)
