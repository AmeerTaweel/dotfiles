;; Disable line numbers for some buffer types
;; (let ((modes '(shell-mode-hook
;;	       term-mode-hook
;;	       eshell-mode-hook)))
;;  (dolist (mode modes)
;;    (add-hook mode (lambda () (display-line-numbers-mode 0)))))

;; Make sure some modes start in Emacs state
;; TODO: Split this out to other configuration modules?
;; (dolist (mode '(custom-mode
   ;;             eshell-mode
     ;;           term-mode))
 ;; (add-to-list 'evil-emacs-state-modes mode))

;; (require-relative "pdf")
