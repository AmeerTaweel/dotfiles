" # General

" Enable syntax highlighting
syntax enable

" Enable filetype detection for plugins and indentation
filetype plugin indent on

" Use UTF8 Encoding
set encoding=UTF-8

" Set characters-per-line-limit
set textwidth=80

" Show column indicating the characters-per-line-recommended-limit
set colorcolumn=81

" Allow leaving buffers without saving, leaving buffer in background
set hidden

" Do not wrap lines
set nowrap

" Always show status line for all windows
set laststatus=2

" Confirm operations that would fail normally (like closing an unsaved buffer)
set confirm

" Allow backspacing over indention, line breaks and insertion start
set backspace="indent,eol,start"

" Enable mouse support for scrolling and resizing
set mouse="a"

" Make mouse work properly inside Tmux
set ttymouse=xterm2

" Set the window’s title, reflecting the file currently being edited
set title

" Enable expanding commands with the tab key
set wildmenu

" Enable folding
set foldmethod=syntax

" Split to the right and down
set splitright
set splitbelow

" Set the number of saved commands in history
set history=10000

" Show incomplete commands at the bottom
set showcmd

" Ignore file’s mode lines
set nomodeline

" Use a block cursor in insert mode
set guicursor="i:block"

" Auto-completion options
set completeopt = "menuone,noinsert,noselect,preview"

" Use diff mode vertical split
set diffopt+=vertical

" Remove the Netrw banner
let g:netrw_banner = 0

" Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
" delays and poor user experience.
set updatetime=100

" Don't pass messages to |insertion-completion-menu|.
set shortmess+=c

" # Hybrid Line Numbers

" Show current line number
set number

" Relative line numbers
set relativenumber

" # Search

" Select items found in search
set incsearch

" Highlight searches by default
set hlsearch

" Ignore case when searching
set ignorecase

" Unless you type a capital
set smartcase

" # Indentation

" Enable auto-indentation
set autoindent

" Enable smart-indentation
set smartindent

" Use tabs not spaces
set noexpandtab

" Set tab width equal to 4 spaces
set tabstop=4
set shiftwidth=4
set softtabstop=0 " Use hard tabs always

" # Temporary Files

" ## Undo

" Enable persistent undo
set undofile

" Set undo files directory
set undodir=/tmp/.vim/undo//

" Create directory automatically if it does not exist
if !isdirectory(expand(&undodir))
	call mkdir(expand(&undodir), "p")
endif

" ## Backup

" Enable backup files
set backup

" Set backup files directory
set backupdir=/tmp/.vim/backup//

" Create directory automatically if it does not exist
if !isdirectory(expand(&backupdir))
	call mkdir(expand(&backupdir), "p")
endif

" ## Swap

" Enable swap files
set swapfile

" Set swap files directory
set directory=/tmp/.vim/swap//

" Create directory automatically if it does not exist
if !isdirectory(expand(&directory))
	call mkdir(expand(&directory), "p")
endif

" # Auto Commands and Groups

" Automatically re-balance windows on resize
autocmd VimResized * :wincmd =

" Disable auto-commenting
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

" Set tab width equal to 4 spaces
autocmd FileType * setlocal tabstop=4 shiftwidth=4 softtabstop=0

" TMUX filetype detection
augroup TmuxFixFileType
	autocmd!
	autocmd BufRead,BufNewFile .tmux.conf set filetype=tmux
	autocmd BufRead,BufNewFile *.tmux set filetype=tmux
augroup end
