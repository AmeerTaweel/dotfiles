""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" # Vim-Plug Package Manager Configuration
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Install Vim-Plug if not found
" The "curl" and "git" commands must be installed on the system for this to work
if empty(glob("~/.vim/autoload/plug.vim"))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
endif

" Specify a directory for plugins
call plug#begin("~/.vim/plugged")

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ## Themes
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" OneHalf
Plug 'sonph/onehalf', { 'rtp': 'vim' }

" Gruvbox
Plug 'gruvbox-community/gruvbox'

" Dracula
Plug 'dracula/vim', { 'as': 'dracula' }

" Nord
Plug 'arcticicestudio/nord-vim'

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ## Git and Version Control
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Fugitive: Git for Vim
Plug 'tpope/vim-fugitive'

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ## Code Formatting, Linting, and Completion
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Conquer of Completion: LSP and Auto Completion
Plug 'neoclide/coc.nvim'
source ~/.config/vim/plugins-config/coc.vim

" Vim Polyglot: Syntax and indentation support for many languages
Plug 'sheerun/vim-polyglot'

" Vim Commentary: Comments for Vim
Plug 'tpope/vim-commentary'

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ## Others
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Vim Tmux Navigator: Better vim and tmux navigation
" Also installed in Tmux with TPM
Plug 'christoomey/vim-tmux-navigator'
source ~/.config/vim/plugins-config/vim-tmux-navigator.vim


" Vim Airline: Vim statusbar
Plug 'vim-airline/vim-airline'

" Window Swap: Swap split windows with ease
Plug 'wesQ3/vim-windowswap'
source ~/.config/vim/plugins-config/window-swap.vim

" Fuzzy Finder: File finder
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

" Vim Eunuch: Unix Shell commands in Vim
Plug 'tpope/vim-eunuch'

" Vim Visual Multi: Multiple Cursors
Plug 'mg979/vim-visual-multi', {'branch': 'master'}

" Vim Surround: Auto Complete Surrounding
Plug 'tpope/vim-surround'

" Vim Targets: More text objects to operate on
Plug 'wellle/targets.vim'
" Cheatsheet
" https://github.com/wellle/targets.vim/blob/master/cheatsheet.md

" ZoomWin: Zoom windows
Plug '~/.vim/plugged-manual/ZoomWin'

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ## Language-Specific
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" ### Python

" Simpyl Fold: Simple Python Code Folding
Plug 'tmhedberg/SimpylFold'


" ### JSON-C

" JSON-C Support for VIM
Plug 'kevinoid/vim-jsonc'

" ### TMUX Config

" Syntax highlighting for TMUX config files
Plug 'tmux-plugins/vim-tmux'

" ### SXHKD

" Syntax Highlighting for sxhkdrc
Plug 'baskerville/vim-sxhkdrc'

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Initialize plugin system
call plug#end()

" Run PlugInstall if there are missing plugins
autocmd VimEnter * if len(filter(values(g:plugs), "!isdirectory(v:val.dir)"))
  \| PlugInstall --sync | source $MYVIMRC
\| endif
