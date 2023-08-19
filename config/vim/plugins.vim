" Current script's directory
let s:vimrcdir = expand("<sfile>:p:h")

" Statusbar
exec "source" . s:vimrcdir . "/" . "plugin-config/lightline.vim"

" Swap split windows with ease
exec "source" . s:vimrcdir . "/" . "plugin-config/window-swap.vim"
