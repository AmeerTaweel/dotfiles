""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" # Vim-Plug Package Manager Configuration
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Install Vim-Plug if it is not installed
let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
  silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" Specify a directory for plugins
call plug#begin('~/.config/nvim/plugged')

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ## Themes
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

Plug 'sonph/onehalf', { 'rtp': 'vim' }

Plug 'gruvbox-community/gruvbox'

Plug 'dracula/vim', { 'as': 'dracula' }

Plug 'arcticicestudio/nord-vim'

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ## Git and Version Control
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Fugitive: Git for Vim
Plug 'tpope/vim-fugitive'

" Vim Gitgutter: Git diff in sign column
Plug 'airblade/vim-gitgutter'
source ~/.config/nvim/plugin-config/vim-gitgutter.vim

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ## Others
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Vim Airline: Vim status bar
Plug 'vim-airline/vim-airline'

" Vim Tmux Navigator: Better vim and tmux navigation
" Also installed in Tmux with TPM
Plug 'christoomey/vim-tmux-navigator'
source ~/.config/nvim/plugin-config/tmux-navigator.vim

" Telescope: Fuzzy Finder
Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'
Plug 'nvim-telescope/telescope-fzy-native.nvim'

" Window Swap: Swap split windows with ease
Plug 'wesQ3/vim-windowswap'
source ~/.config/nvim/plugin-config/window-swap.vim

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ## Code Formatting, Linting, and Completion
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Vim Commentary: Comments for Vim
Plug 'tpope/vim-commentary'

" ALE: Check Syntax in Vim Async
Plug 'dense-analysis/ale'

" You Complete Me: Auto Completion Suggestions
" Plug 'ycm-core/YouCompleteMe'

" Eclim: Java Code Completion
Plug 'ervandew/eclim'

Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Vim Multiple Cursors: Multiple Cursors
Plug 'terryma/vim-multiple-cursors'

" Vim Eunuch: Unix Shell commands in Vim
Plug 'tpope/vim-eunuch'

" Vim Surround: Auto Complete Surrounding
Plug 'tpope/vim-surround'

" Emmet: Emmet for Vim
Plug 'mattn/emmet-vim'

" Simpyl Fold: Simple Python Code Folding
Plug 'tmhedberg/SimpylFold'

" Vim Targets: More text objects to operate on
Plug 'wellle/targets.vim'

" Initialize plugin system
call plug#end()

" Run PlugInstall if there are missing plugins
autocmd VimEnter * if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
	\| PlugInstall --sync | source $MYVIMRC
	\| endif
