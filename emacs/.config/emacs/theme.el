;;;; Modus Vivendi Theme

;;; Imports

(require 'load-relative)

;;; Syntax Highlighting

;; Increase the number of bolded syntax elements
(customize-set-variable 'modus-themes-bold-constructs t)
;; Increase the number of italicized syntax elements
(customize-set-variable 'modus-themes-italic-constructs t)
;; Make matching parenthesis more clear
(customize-set-variable 'modus-themes-paren-match '(bold intense))

;;; Org Mode

;; Make headings in org files more distinct
(customize-set-variable 'modus-themes-headings '((t . (background bold rainbow 1))))
;; Tint the background of code blocks in org files
(customize-set-variable 'modus-themes-org-blocks 'tinted-background)

;;; Other

;; Remove mode-line border
(customize-set-variable 'modus-themes-mode-line 'borderless)

;; Keep syntax-highlighting for selection
(customize-set-variable 'modus-themes-region 'bg-only)

;; Enhance minibuffer completions
(customize-set-variable 'modus-themes-completions 'minimal)

;; Better highlighting for current line number
(customize-set-variable 'modus-themes-subtle-line-numbers t)

;;; Load Theme

(load-theme 'modus-vivendi t)

;;; Module End

(provide-me)
