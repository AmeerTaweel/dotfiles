" Use Vim settings rather than Vi settings
set nocompatible

" Vim Plug Package Manager Config

" Specify a directory for plugins
call plug#begin('~/.vim/plugged')

" Themes

" Dracula: Theme
Plug 'dracula/vim', { 'as': 'dracula' }

" Gruvbox: Theme
Plug 'morhetz/gruvbox'

" OneHalf: Theme
Plug 'sonph/onehalf', { 'rtp': 'vim' }

" Vim Airline: Vim statusbar
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Fugitive: Git for Vim
Plug 'tpope/vim-fugitive'

" NERDTree: Tree explorer plugin for Vim
Plug 'scrooloose/nerdtree'

" Fuzzy Finder: File finder
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

" AG: Silver Searcher for Vim
Plug 'rking/ag.vim'

" Vim Multiple Cursors: Multiple Cursors
Plug 'terryma/vim-multiple-cursors'

" Vim Eunuch: Unix Shell commands in Vim
Plug 'tpope/vim-eunuch'

" Vim Surround: Auto Complete Surrounding
Plug 'tpope/vim-surround'

" Editor Config: Global Configuration For All Editors
Plug 'editorconfig/editorconfig-vim'

" Emmet: Emmet for Vim
Plug 'mattn/emmet-vim'

" Vim Gitgutter: Git diff in sign column
Plug 'airblade/vim-gitgutter'

" ALE: Check Syntax in Vim Async
Plug 'dense-analysis/ale'

" Vim Commentary: Comments for Vim
Plug 'tpope/vim-commentary'

" Window Swap: Swap split windows with ease
Plug 'wesQ3/vim-windowswap'

" You Complete Me: Auto Completion Suggestions
Plug 'ycm-core/YouCompleteMe'

" Simpyl Fold: Simple Python Code Folding
Plug 'tmhedberg/SimpylFold'

" Initialize plugin system
call plug#end()

" Keybindings

" Leader Key
let mapleader = ","
" Terminal Normal Mode
tnoremap <esc> <c-w>N
" Saving and Quitting
nnoremap <leader>s :w<cr>
nnoremap <leader>q :q<cr>
nnoremap <leader>x :x<cr>
nnoremap <leader>aq :qa<cr>
nnoremap <leader>as :wa<cr>
nnoremap <leader>ax :xa<cr>
nnoremap <leader><leader>q :q!<cr>
nnoremap <leader><leader>aq :qa!<cr>
" Window Navigation
nnoremap <leader>wk <c-w>k
nnoremap <leader>wl <c-w>l
nnoremap <leader>wj <c-w>j
nnoremap <leader>wh <c-w>h
" Semi-colon instead of colon for commands
nnoremap ; :
vnoremap ; :
" Open .vimrc
nnoremap <leader>rc :e $MYVIMRC<cr>
nnoremap <leader>hrc :split $MYVIMRC<cr>
nnoremap <leader>vrc :vsplit $MYVIMRC<cr>
" Enter creates a new line in normal mode
nnoremap <cr> o<esc>
" Enable folding with the space bar
nnoremap <space> za
" Enable spell checking, English in US style
nnoremap <leader>spl :set spell! spelllang=en_us<cr>
" Remove highlighed search results
nnoremap <leader>noh :noh<cr>
" Toggle Wrapping
nnoremap <leader>tw :set wrap!<cr>
" Show registers
nnoremap <leader>rg :reg<cr>
" Manually source .vimrc
nnoremap <leader>src :source ~/.vimrc<cr>
" Fuzzy Finder Mappings
nnoremap <leader>f :Files<cr>
" NERDTree Mappings
nnoremap <leader>b :NERDTreeToggle<cr>
" Vim Gitgutter Mappings
nnoremap <leader>ggh :GitGutterLineHighlightsToggle<cr>
" Fugitive Mappings
nnoremap <leader>gac :Git add %<cr>
nnoremap <leader>gaa :Git add .<cr>
nnoremap <leader>gc :Git commit<cr>
nnoremap <leader>gps :Git push<cr>
nnoremap <leader>gpl :Git pull<cr>
nnoremap <leader>glg :Git log<cr>
nnoremap <leader>gst :Git status<cr>
" ALE Mappings
nnoremap <silent> <leader>k :ALEPrevious<cr>
nnoremap <silent> <leader>j :ALENext<cr>
" YouCompleteMe Mappings
nnoremap <leader>to :YcmCompleter GoTo<cr>
nnoremap <leader>tf :YcmCompleter GoToReferences<cr>
nnoremap <leader>doc :YcmCompleter GetDoc<cr>
nnoremap <leader>rf :YcmCompleter RefactorRename<space>

" Theme

" Current Theme
colorscheme onehalfdark

" Other Config

" Enable folding
set foldmethod=indent
set foldlevel=99
" Enable syntax highlighting
syntax enable
" Show line numbers (Hybrid Line Numbers)
set number relativenumber
augroup numbertoggle
	" Make line numbers absolute when in insert mode and on buffer leaving
	autocmd!
	autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
	autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber
augroup END
" Select items found in search
set incsearch
" Highlight searches by default
set hlsearch
" Ignore case when searching
set ignorecase
" Unless you type a capital
set smartcase
" Enable Auto Indentation
filetype indent on
" Enable expanding commands with the tab key
set wildmenu
" Highlight cursor line and column
set cursorline
set cursorcolumn
" Allow backspacing over indention, line breaks and insertion start
set backspace=indent,eol,start
" Set bigger history for executed commands
set history=1000
" Show incomplete commands at the bottom
set showcmd
" Automatically re-read files if modified outside Vim
set autoread
" Enable mouse support for scrolling and resizing
set mouse=a
" Set the window’s title, reflecting the file currently being edited
set title
" Save backup, swap, and undo files in the /tmp directory.
" This is cleaner than saving them in the current working directory.
set backupdir=/tmp//
set directory=/tmp//
set undodir=/tmp//
" Wrap lines at convenient points, avoid wrapping in the middle of a word
set linebreak
" Ignore file’s mode lines; use vimrc configurations instead
set nomodeline
" Enable Presistent Undo
set undofile

" ALE

" ALE Global Configuration
let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
