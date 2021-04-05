""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" # Settings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Use UTF-8 Encoding
set encoding=UTF-8

" Enabling Plugin and Indent
filetype plugin indent on

" Enable syntax highlighting
syntax enable

" Set characters-per-line-limit
set textwidth=80

" Show column indicating the characters-per-line-recommended-limit
set colorcolumn=80

" Enable spell checking
set spell spelllang=en_us

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ## Hybrid Line Numbers
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Relative line numbers
set relativenumber

" Show current line number
set number

" Make line numbers absolute when in insert mode and on buffer leaving
augroup LineNumberDisplayToggle
	autocmd!
	autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
	autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber
augroup END

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ## Search
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Select items found in search
set incsearch

" Highlight searches by default
set hlsearch

" Ignore case when searching
set ignorecase

" Unless you type a capital
set smartcase

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ## Indentation
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Enable indent
filetype indent on

" Enable auto-indentation
set autoindent

" Enable smart-indentation
set smartindent

" Use tabs not spaces
set noexpandtab

" Set tab width equal to 4 spaces
set shiftwidth=4 tabstop=4 softtabstop=4

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Highlight cursor line and column
set cursorline
set cursorcolumn

" Enable true color
set termguicolors

" Do not wrap lines
set nowrap

" Always show status line for all windows
set laststatus=2

" Confirm operations that would fail normally (like closing an unsaved buffer)
set confirm

" Allow backspacing over indention, line breaks and insertion start
set backspace=indent,eol,start

" Disable auto-commenting
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

" Automatically re-read files if modified outside the editor
set autoread

" Enable mouse support for scrolling and resizing
set mouse=a

" Set the window’s title, reflecting the file currently being edited
set title

" Enable expanding commands with the tab key
set wildmenu

" Enable folding using vim treesitter
set foldmethod=expr
set foldexpr=nvim_treesitter#foldexpr()

" Split to the right and down
set splitright
set splitbelow

" Set the number of saved commands in history
set history=10000

" Show incomplete commands at the bottom
set showcmd

" Save backup, swap, and undo files in the /tmp/nvim directory.
" This is cleaner than saving them in the current working directory.
set backupdir=/tmp/nvim//
set directory=/tmp/nvim//
set undodir=/tmp/nvim//

" Ignore file’s mode lines
set nomodeline

" Enable Persistent Undo
set undofile

" Always show sign column
set signcolumn=yes

" Use a block cursor in insert mode
set guicursor=i:block

" Automatically re-balance windows on resize
autocmd VimResized * :wincmd =

" Make folds persistent
augroup AutoSaveFolds
	autocmd!
	" View files are about 500 bytes
	" Bufleave but not BufWinLeave captures closing 2nd tab
	" Nested is needed by BufWrite* (if triggered via other autocmd)
	autocmd BufWinLeave,BufLeave,BufWritePost ?* nested silent! mkview!
	autocmd BufWinEnter ?* silent! loadview
augroup end

" Syntax highlighting for Tmux configuration files
autocmd BufRead,BufNewFile *.tmux.conf.* set filetype=tmux

" Expand tabs to spaces in Python code
autocmd FileType python setlocal expandtab
