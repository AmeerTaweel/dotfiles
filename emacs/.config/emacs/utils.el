;;;; utils module

;; functions

(defun utils//disable-line-numbers-in-mode (mode)
  "Disable line numbers in a mode."
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(defun utils//clear-keymap (keymap)
  "Delete all keybindings in a keymap."
  (setq keymap (make-sparse-keymap)))

(provide-me) ; end of module
