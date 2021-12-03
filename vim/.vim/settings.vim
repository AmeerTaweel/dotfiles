" # Settings

" ## General

" Enable syntax highlighting
syntax enable

" Enable filetype detection for plugins and indentation
filetype plugin indent on

" Use UTF-8 Encoding
let &encoding = "UTF-8"

" Set characters-per-line-limit
let &textwidth = 80

" Show column indicating the characters-per-line-recommended-limit
let &colorcolumn = &textwidth + 1

" Allow leaving buffers without saving, leaving buffer in background
let &hidden = 1

" Do not wrap lines
let &wrap = 0

" Always show status line for all windows
let &laststatus = 2

" Confirm operations that would fail normally (like closing an unsaved buffer)
let &confirm = 1

" Allow backspacing over indention, line breaks and insertion start
let &backspace = "indent,eol,start"

" Enable mouse support for scrolling and resizing
let &mouse = "a"

" Set the window’s title, reflecting the file currently being edited
let &title = 1

" Enable expanding commands with the tab key
let &wildmenu = 1

" Enable folding
let &foldmethod = "syntax"

" Split to the right and down
let &splitright = 1
let &splitbelow = 1

" Set the number of saved commands in history
let &history = 10000

" Show incomplete commands at the bottom
let &showcmd = 1

" Ignore file’s mode lines
let &modeline = 0

" Use a block cursor in insert mode
let &guicursor = "i:block"

" Auto-completion options
let &completeopt = "menuone,noinsert,noselect,preview"

" Use diff mode vertical split
let &diffopt = "internal,filler,closeoff, vertical"

" Remove the Netrw banner
let g:netrw_banner = 0

" Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
" delays and poor user experience.
let &updatetime = 100

" Don't pass messages to |insertion-completion-menu|.
let &shortmess = "filnxtToOSAc"

" ## Hybrid Line Numbers

" Relative line numbers
let &relativenumber = 1

" Show current line number
let &number = 1

" ## Search

" Select items found in search
let &incsearch = 1

" Highlight searches by default
let &hlsearch = 1

" Ignore case when searching
let &ignorecase = 1

" Unless you type a capital
let &smartcase = 1

" ## Indentation

" Enable auto-indentation
let &autoindent = 1

" Enable smart-indentation
let &smartindent = 1

" Use tabs not spaces
let &expandtab = 0

" Set tab width equal to 4 spaces
let &tabstop = 4
let &shiftwidth = &tabstop
let &softtabstop = &tabstop

" ## Temporary Files

" ### Undo

" Enable persistent undo
let &undofile = 1

" Set undo files directory
let &undodir = "/tmp/.vim/undo"

" Create directory automatically if it does not exist
if !isdirectory(expand(&undodir))
	call mkdir(expand(&undodir), "p")
endif

" ### Backup

" Enable backup files
let &backup = 1

" Set backup files directory
let &backupdir = "/tmp/.vim/backup"

" Create directory automatically if it does not exist
if !isdirectory(expand(&backupdir))
	call mkdir(expand(&backupdir), "p")
endif

" ### Swap

" Enable swap files
let &swapfile = 1

" Set swap files directory
let &directory = "/tmp/.vim/swap"

" Create directory automatically if it does not exist
if !isdirectory(expand(&directory))
	call mkdir(expand(&directory), "p")
endif

" ## Auto Commands and Groups

" Automatically re-balance windows on resize
autocmd VimResized * :wincmd =

" Disable auto-commenting
autocmd FileType * let &formatoptions = "jql"

" TMUX filetype detection
augroup TmuxFixFileType
	autocmd!
	autocmd BufRead,BufNewFile .tmux.conf let &filetype = "tmux"
	autocmd BufRead,BufNewFile *.tmux let &filetype = "tmux"
augroup end
