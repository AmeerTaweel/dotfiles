;;;; Modus Vivendi Theme

;;; Imports

(require 'use-package)
(require 'load-relative)

;;; Customizations

(use-package emacs
  :custom
  ;; Syntax Highlighting
  ;; Increase the number of bolded syntax elements
  (modus-themes-bold-constructs t)
  ;; Increase the number of italicized syntax elements
  (modus-themes-italic-constructs t)
  ;; Make matching parenthesis more clear
  (modus-themes-paren-match '(bold intense))

  ;; Org Mode
  ;; Make headings in org files more distinct
  (modus-themes-headings '((t . (background bold rainbow 1))))
  ;; Tint the background of code blocks in org files
  (modus-themes-org-blocks 'tinted-background)

  ;; Other
  ;; Remove mode-line border
  (modus-themes-mode-line 'borderless)
  ;; Keep syntax-highlighting for selection
  (modus-themes-region 'bg-only)
  ;; Enhance minibuffer completions
  (modus-themes-completions 'minimal)
  ;; Better highlighting for current line number
  (modus-themes-subtle-line-numbers t)
  :config
  (load-theme 'modus-vivendi t))

;;; Module End

(provide-me)
