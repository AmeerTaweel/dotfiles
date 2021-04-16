""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" # TMUX Settings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Make mouse work inside Tmux properly
set ttymouse=xterm2

" Syntax highlighting for Tmux configuration files
autocmd BufRead,BufNewFile *.tmux.conf.* set filetype=tmux

" Fix slow window switching inside Tmux
set shell=/bin/bash\ -i
