""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" # TMUX Settings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Make mouse work inside Tmux properly
set ttymouse=xterm2

" Syntax highlighting for Tmux configuration files
augroup TmuxFixFileType
	autocmd!
	autocmd BufRead,BufNewFile .tmux.conf set filetype=tmux
	autocmd BufRead,BufNewFile *.tmux set filetype=tmux
augroup end
