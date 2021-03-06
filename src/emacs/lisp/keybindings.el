;-------------------------------------------------------------------------------
; # GENERAL - Plugin Configuration
;-------------------------------------------------------------------------------

;; Define the leader key
(general-create-definer leader-def
  :prefix ",")

(general-create-definer leader-leader-def
  :prefix ",,")

;; Window navigation
(general-def
 "C-l" 'evil-window-right
 "C-k" 'evil-window-up
 "C-j" 'evil-window-down
 "C-h" 'evil-window-left)

;; We are usin C-l to move to the right window
;; So map the help command to M-h
(general-def
 "M-h" 'help-command)

(defun save-all ()
  "Save all buffers"
  (interactive)
  (save-some-buffers t))

(defun save-and-quit-all ()
  "Save and quit all buffers"
  (interactive)
  (save-all)
  (evil-quit-all))

;; Org mode
(leader-def
  :states '(normal visual)
  "o"  '(:ignore o :which-key "org mode")
  "oa" '(org-agenda :which-key "agenda")
  "ol" '(org-agenda-list :which-key "agenda list")
  "oc" '(org-capture :which-key "capture"))

;; Saving and Quitting
(leader-def
  :states '(normal visual)
  "q" '(delete-window :which-key "quit current window")
  "s" '(save-buffer :which-key "save current buffer")
  "x" '(evil-save-and-close :which-key "save current buffer and close current window")
  "d" '(kill-buffer-and-window :which-key "delete current buffer"))

(leader-def
  :states '(normal visual)
  "a"  '(:ignore a :which-key "apply to all")
  "aq" '(evil-quit-all :which-key "quit all")
  "as" '(save-all :which-key "save all")
  "ax" '(save-and-quit-all :which-key "save and quit all"))

;; Fuzzy Finding
(leader-def
  :states '(normal visual)
  "f"  '(:ignore f :which-key "fuzzy finding")
  "ff" '(counsel-find-file :which-key "find file")
  "fg" '(counsel-rg :which-key "grep content")
  "fb" '(counsel-switch-buffer :which-key "find buffer")
  "gf" '(counsel-git :which-key "find file in git repo")
  "gg" '(counsel-git-grep :which-key "grep content in git repo")
  "fl" '(swiper :which-key "fuzzy find buffer content"))

;; Move Line/Selection Up/Down
(general-nmap "K" "ddkP")
(general-nmap "J" "ddp")

(general-vmap "K" "dkP'[V']")
(general-vmap "J" "dp'[V']")

(defun open-init-el ()
  "Opens the default init.el in a new buffer."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun load-init-el ()
  "Loads the default init.el."
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;; Dotfiles
(leader-def
  :states '(normal visual)
  "erc" '(open-init-el :which-key "access init.el"))

(leader-leader-def
  :states '(normal visual)
  "r" '(load-init-el :which-key "load init.el"))

;; File Explorer
(defun open-dired-in-current-window ()
  "Opens dired in the current window."
  (interactive)
  (dired "."))

(defun open-dired-in-horizontal-split ()
  "Opens dired in a horizontal split."
  (interactive)
  (split-window-below)
  (open-dired-in-current-window))

(defun open-dired-in-vertical-split ()
  "Opens dired in a vertical split."
  (interactive)
  (split-window-right)
  (open-dired-in-current-window))

(leader-def
  :states '(normal visual)
  "exc" '(open-dired-in-current-window :which-key "open file explorer in current buffer")
  "exs" '(open-dired-in-horizontal-split :which-key "open file explorer in horizontal split")
  "exv" '(open-dired-in-vertical-split :which-key "open file explorer in vertical split"))

;; Splitting windows
(defun horizontal-split ()
  "Create a horizontal split and balance windows."
  (interactive)
  (split-window-below)
  (balance-windows))

(defun vertical-split ()
  "Create a vertical split and balance windows."
  (interactive)
  (split-window-right)
  (balance-windows))

(leader-def
  :states '(normal visual)
  "-" '(horizontal-split :which-key "open horizontal split (below)")
  "/" '(vertical-split :which-key "open vertical split (right)"))

;; Tabs
(general-nvmap "tn" "C-x t 2")
(general-nvmap "td" "C-x t 0")

;; Hydra
(leader-def
  :states '(normal visual)
  "z"  '(:ignore z :which-key "zoom")
  "zt" '(hydra-scale-text/body :which-key "scale text (hydra)")
  "zw" '(hydra-scale-window/body :which-key "scale window (hydra)"))

;; Other mappings
(general-define-key
 :states 'motion
 ";" 'evil-ex)

(defun evil-normal-mode-insert-line ()
  "Insert a new line in normal mode."
  (interactive)
  (evil-insert-newline-below))

(general-nmap "RET" 'evil-normal-mode-insert-line)

(general-nvmap "C-b" 'balance-windows)
(general-imap "C-h" 'evil-delete-backward-char-and-join)

(leader-def
  :states '(normal visual)
  "p"  '(projectile-command-map :which-key "projectile"))

;; " Enable folding with the space bar
;; nnoremap <space> za
;; 
;; " Remove highlighted search results
;; nnoremap <leader>noh :noh<cr>
;; 
;; " Show registers
;; nnoremap <leader>rg :reg<cr>
;; 
;; " Make . to work with visually selected lines
;; vnoremap . :normal.<cr>
;; 
;; " Toggle spell checking
;; nnoremap <leader>ts :set spell! spelllang=en_us<cr>
;; 
;; " Window Swap: Swap Window
;; nnoremap <leader>ww :call WindowSwap#EasyWindowSwap()<cr>
;; 
;; """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
;; """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
;; " ## Zooming
;; """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
;; 
;; nnoremap <silent><leader>z :MaximizerToggle<cr>
;; vnoremap <silent><leader>z :MaximizerToggle<cr>gv
;; inoremap <silent><leader>z <c-o>:MaximizerToggle<cr>
;; 
;; """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
;; 
;; """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
;; " ## Git and Version Control
;; """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
;; 
;; " Vim Fugitive
;; nnoremap <leader>gac :Git add %<cr>
;; nnoremap <leader>gaa :Git add .<cr>
;; nnoremap <leader>gc :Git commit<cr>
;; nnoremap <leader>gps :Git push<cr>
;; nnoremap <leader>gpl :Git pull<cr>
;; nnoremap <leader>glg :Git log<cr>
;; nnoremap <leader>gst :Git status<cr>
;; "" Opens git status with the ability to stage and unstage commits
;; ""	s -> stage
;; ""	u -> unstage
;; ""	= -> see diff
;; ""	cc -> commit
;; nnoremap <leader>gm :G<cr>
;; 
;; " Vim GitGutter
;; " ]c -> Next Hunk
;; " [c -> Previous Hunk
;; " <leader>hs -> Stage Hunk
;; " <leader>hu -> Undo Hunk
;; " <leader>hp -> Preview Hunk
;; " ic -> In Hunk
;; " ac -> Around Hunk
;; 
;; """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
;; 
;; """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
;; " ## Code Formatting and Navigation
;; """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
;; 
;; " ALE Mappings
;; "" Fix code with ALE
;; nnoremap <leader>F :ALEFix<cr>
;; "" Move to next and previous error with ALE
;; nnoremap <silent> <leader>ep :ALEPrevious<cr>
;; nnoremap <silent> <leader>en :ALENext<cr>
;; 
;; " YouCompleteMe Mappings
;; nnoremap <leader>to :YcmCompleter GoTo<cr>
;; nnoremap <leader>tf :YcmCompleter GoToReferences<cr>
;; nmap <c-k> <plug>(YCMHover)
;; nnoremap <leader>doc :YcmCompleter GetDoc<cr>
;; nnoremap <leader>rf :YcmCompleter RefactorRename<space>
;; 
;; """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
