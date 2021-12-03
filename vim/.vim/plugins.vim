" # Vim-Plug Package Manager Configuration

" Install Vim-Plug if not found
" The "curl" and "git" commands must be installed on the system for this to work
if empty(glob("~/.vim/autoload/plug.vim"))
	silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
	\ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
endif

" Specify a directory for plugins
call plug#begin("~/.vim/plugged")

" Vim Lightline: Vim statusbar
Plug 'itchyny/lightline.vim'
source ~/.vim/plugin-config/lightline.vim

" Vim Tmux Navigator: Better vim and tmux navigation
" Also installed in Tmux with TPM
Plug 'christoomey/vim-tmux-navigator'
source ~/.vim/plugin-config/tmux-navigator.vim

" Window Swap: Swap split windows with ease
Plug 'wesQ3/vim-windowswap'
source ~/.vim/plugin-config/window-swap.vim

" Fugitive: Git for Vim
Plug 'tpope/vim-fugitive'

" Vim Eunuch: Unix Shell commands in Vim
Plug 'tpope/vim-eunuch'

" Vim Polyglot: Syntax and indentation support for many languages
Plug 'sheerun/vim-polyglot'

" Vim Commentary: Comments for Vim
Plug 'tpope/vim-commentary'

" Vim Targets: More text objects to operate on
" Cheatsheet: https://github.com/wellle/targets.vim/blob/master/cheatsheet.md
Plug 'wellle/targets.vim'

" Vim Surround: Quoting and parenthesizing made simple
Plug 'tpope/vim-surround'

" Vim Repeat: Enable repeating supported plugin maps with the "." operator
Plug 'tpope/vim-repeat'

" Initialize plugin system
call plug#end()

" Run PlugInstall if there are missing plugins
autocmd VimEnter * if len(filter(values(g:plugs), "!isdirectory(v:val.dir)"))
	\| PlugInstall --sync | source $MYVIMRC
\| endif
