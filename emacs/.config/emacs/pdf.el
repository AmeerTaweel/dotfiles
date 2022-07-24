;;;; pdf module

;; imports

(require-relative "utils")

;; functions

(defun pdf//goto-last-page (&optional page)
  "`evil' wrapper around `pdf-view-last-page'."
  (interactive "P")
  (if page
      (pdf-view-goto-page page)
    (pdf-view-last-page))
  (image-bob))

(defun pdf//goto-first-page (&optional page)
  "`evil' wrapper around `pdf-view-first-page'."
  (interactive "P")
  (if page
      (pdf-view-goto-page page)
    (pdf-view-first-page))
  (image-bob))

(defun pdf//goto-next-page (&optional count)
  "`evil' wrapper around `pdf-view-next-page'."
  (interactive "P")
  (pdf-view-next-page (if count count 1))
  (image-bob))

(defun pdf//goto-previous-page (&optional count)
  "`evil' wrapper around `pdf-view-previous-page'."
  (interactive "P")
  (pdf-view-previous-page (if count count 1))
  (image-bob))

;; package configuration

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :init
  (utils//disable-line-numbers-in-mode 'pdf-view-mode-hook)
  (add-hook 'pdf-view-mode-hook #'pdf-tools-enable-minor-modes)
  ;; start in normal mode
  (evil-set-initial-state 'pdf-view-mode 'normal)
  (evil-set-initial-state 'pdf-outline-minor-mode 'normal)
  ;; clear default keybindings
  (utils//clear-keymap 'pdf-view-mode-map)
  (utils//clear-keymap 'pdf-outline-buffer-mode-map)
  :general
  ;; no-prefix normal keybindings
  (general-define-key
   :states 'normal
   :keymaps 'pdf-view-mode-map
   ;; motion
   "h" '(image-backward-hscroll :which-key "scroll left")
   "j" '(pdf-view-next-line-or-next-page :which-key "scroll down")
   "k" '(pdf-view-previous-line-or-previous-page :which-key "scroll up")
   "l" '(image-forward-hscroll :which-key "scroll right")
   "<down>" '(pdf-view-next-line-or-next-page :which-key "scroll down")
   "<up>" '(pdf-view-previous-line-or-previous-page :which-key "scroll up")
   "C-d" '(pdf-view-next-line-or-next-page :which-key "scroll down")
   "C-u" '(pdf-view-previous-line-or-previous-page :which-key "scroll up")
   "J" '(pdf//goto-next-page :which-key "go to next page")
   "K" '(pdf//goto-previous-page :which-key "go to previous page")
   "gg" '(pdf//goto-first-page :which-key "go to first page")
   "G" '(pdf//goto-last-page :which-key "go to page")
   "0" '(image-bol :which-key "go to right edge of page")
   "^" '(image-bol :which-key "go to right edge of page")
   "$" '(image-eol :which-key "go to left edge of page")
   ;; mark
   "m" '(pdf-view-position-to-register :which-key "mark position")
   "'" '(pdf-view-jump-to-register :which-key "go to mark")
   ;; zoom
   "+" '(pdf-view-enlarge :which-key "zoom in")
   "-" '(pdf-view-shrink :which-key "zoom out")
   ;; search
   ;; TODO: make search work like in evil mode
   "/" '(isearch-forward :which-key "search forward")
   "?" '(isearch-backward :which-key "search backward")
   "n" '(isearch-repeat-forward :which-key "search repeat forward")
   "N" '(isearch-repeat-backward :which-key "search repeat backward")
   ;; mouse
   "<down-mouse-1>" '(pdf-view-mouse-set-region :which-key "open link")
   "<S-down-mouse-1>" '(pdf-view-mouse-extend-region :which-key "select text")
   "<M-down-mouse-1>" '(pdf-view-mouse-set-region-rectangle :which-key "select region"))
  ;; no-prefix visual keybindings
  (general-define-key
   :states 'visual
   :keymaps 'pdf-view-mode-map
   ;; yank
   "y" '(pdf-view-kill-ring-save :which-key "yank selection")
   "C-S-c" '(pdf-view-kill-ring-save :which-key "yank selection"))
  ;; prefix normal keybindings
  (general-define-key
   :states 'normal
   :keymaps 'pdf-view-mode-map
   :prefix (concat <leader> <leader>)
   ;; go to
   "gt" '(image-bob :which-key "go to top of page")
   "gb" '(image-eob :which-key "go to bottom of page")
   "go" '(pdf-outline :which-key "go to outline")
   ;; "gl" '(pdf-view-goto-label :which-key "go to label")

   ;; zoom
   "z0" '(pdf-view-scale-reset :which-key "reset zoom")

   ;; scale
   "sw" '(pdf-view-fit-width-to-window :which-key "fit width to window")
   "sh" '(pdf-view-fit-height-to-window :which-key "fit height to window")
   "sp" '(pdf-view-fit-page-to-window :which-key "fit page to window")

   ;; find
   "fl" '(pdf-links-isearch-link :which-key "find link")
   "fo" '(pdf-occur :which-key "find occurences")

   ;; Slices
   ;; "??" '(pdf-view-set-slice-from-bounding-box :which-key "???")
   ;; "??" '(pdf-view-set-slice-using-mouse :which-key "???")
   ;; "??" '(pdf-view-reset-slice :which-key "???")

   ;; Look
   ;; "??" '(pdf-view-dark-minor-mode :which-key "???")
   ;; "??" '(pdf-view-midnight-minor-mode :which-key "???")
   ;; "??" '(pdf-view-printer-minor-mode :which-key "???")

   ;; Other
   ;; "??" '(pdf-view-extract-region-image :which-key "???")
   ;; "??" '(pdf-links-action-perform :which-key "???")
   ;; TODO: setup printing
   ;; https://emacs.stackexchange.com/questions/24552/how-to-print-a-pdf-displayed-with-pdf-tools-from-emacs
   ;; "??" '(pdf-misc-print-document :which-key "???")
   )
  (general-define-key
   :states 'normal
   :keymaps 'pdf-outline-buffer-mode-map
   ;; open
   "<return>" 'pdf-outline-follow-link-and-quit
   "S-<return>" 'pdf-outline-follow-link
   "M-<return>" 'pdf-outline-display-link
   "<down-mouse-1>"	'(pdf-view-mouse-set-region :which-key "open link")
   "<S-down-mouse-1>"	'(pdf-view-mouse-extend-region :which-key "select text")
   "<M-down-mouse-1>"	'(pdf-view-mouse-set-region-rectangle :which-key "select region"))
   "go" 'pdf-outline-follow-link
   "." 'pdf-outline-move-to-current-page
   "SPC" 'pdf-outline-select-pdf-window

   "G" 'pdf-outline-end-of-buffer
   "^" 'pdf-outline-up-heading
   "<" 'pdf-outline-up-heading ; TODO: Don't set this by default?

   "zf" 'pdf-outline-follow-mode ; Helm has "C-c C-f" in Emacs state.

   "<tab>" 'outline-toggle-children
   "<backtab>" 'pdf-outline-toggle-subtree
   "h" '(image-backward-hscroll :which-key "scroll left")))

(provide-me) ; end of module
