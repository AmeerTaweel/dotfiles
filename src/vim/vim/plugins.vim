""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" # Vim-Plug Package Manager Configuration
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Install Vim-Plug if not found
" The "curl" and "git" commands must be installed on the system for this to work
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
endif

" Run PlugInstall if there are missing plugins
autocmd VimEnter * if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
  \| PlugInstall --sync | source $MYVIMRC
\| endif

" Specify a directory for plugins
call plug#begin('~/.vim/plugged')

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
source ~/.config/vim/plugins-config/vim-gitgutter.vim

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ## Code Formatting, Linting, and Completion
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Vim Commentary: Comments for Vim
Plug 'tpope/vim-commentary'

" ALE: Check Syntax in Vim Async
Plug 'dense-analysis/ale'
source ~/.config/vim/plugins-config/asynchronous-lint-engine.vim

" You Complete Me: Auto Completion Suggestions
Plug 'ycm-core/YouCompleteMe'

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ## Others
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Vim Tmux Navigator: Better vim and tmux navigation
" Also installed in Tmux with TPM
Plug 'christoomey/vim-tmux-navigator'
source ~/.config/vim/plugins-config/vim-tmux-navigator.vim

" Tmux.vim: Plugin for vim to enhance dealing with tmux config files
Plug 'tmux-plugins/vim-tmux'

" Vim Airline: Vim statusbar
Plug 'vim-airline/vim-airline'

" Window Swap: Swap split windows with ease
Plug 'wesQ3/vim-windowswap'
source ~/.config/vim/plugins-config/window-swap.vim

" Vim Maximizer: Zooming window
Plug 'szw/vim-maximizer'

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

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ## Language-Specific
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" ### Python

" Simpyl Fold: Simple Python Code Folding
Plug 'tmhedberg/SimpylFold'

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Initialize plugin system
call plug#end()

autocmd VimEnter * if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
	\| PlugInstall --sync | source $MYVIMRC
	\| endif
