;;;; Completion
;;;; Based on the completion module of crafted-emacs

;;; Imports

(require 'use-package)
(require 'load-relative)

(require-relative "keys")

;;; Vertico
;;; Performant and minimalistic vertical completion UI based on the default completion system

(use-package vertico
  :diminish
  :custom
  ; cycle back to top/bottom result when the edge is reached
  (vertico-cycle t)
  :init
  (vertico-mode 1))

(use-package vertico-directory
  :diminish
  :after vertico)

;;; Marginalia
;;; Add marginalia to the minibuffer completions

(use-package marginalia
  :diminish
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode 1))

;;; Consult
;;; Practical commands based on the Emacs completion function

(use-package consult
  :diminish
  :bind
  ([remap switch-to-buffer] . consult-buffer)
  :general
  (general-define-key
   :prefix <prefix-emacs>
   "r" '(consult-recent-file :which-key "find recent file")
   "e" '(consult-file-externally :which-key "open file externally")
   "s" '(consult-line :which-key "find line in buffer"))
  :custom
  (completion-in-region-function #'consult-completion-in-region))

;;; Orderless
;;; Better fuzzy matching

(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-overrides '((file (styles . (partial-completion))))))

;;; Embark
;;; Emacs mini-buffer actions 

(use-package embark
  :custom
  ;; Show bindings in a key prefix with C-h
  (prefix-help-command #'embark-prefix-help-command)
  :bind
  ([remap describe-bindings] . embark-bindings))

(use-package embark-consult
  :hook
  ((embark-collect-mode . consult-preview-at-point-mode)))

;;; Corfu
;;; Intellisense for Emacs

(use-package corfu
  :custom
  ;; Allow cycling through candidate
  (corfu-cycle t)
  ;; Enable auto-completion
  (corfu-auto t)
  ;; Disable candidate preselection
  (corfu-preselect-first nil)
  ;; Complete with less prefix keys
  (corfu-auto-prefix 2)
  ;; No completion delay
  (corfu-auto-delay 0.0)
  ;; Echo docs for current completion option
  (corfu-echo-documentation 0.25)
  :init
  (global-corfu-mode 1))

(use-package corfu-doc
  :general
  (general-define-key
   :keymaps 'corfu-map
   "M-p" 'corfu-doc-scroll-down
   "M-n" 'corfu-doc-scroll-up)
  :hook
  ((corfu-mode . corfu-doc-mode)))

;;; Cape
;;; Intellisense extensions that can be used with the Corfu

(use-package cape
  :init
  ;; Add cape completion sources
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;; Silence the pcomplete capf, no errors or messages
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

(provide-me)
