""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" # Keybindings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Leader Key
let mapleader = ","

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ## Saving and Quitting
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

nnoremap <leader>s :w<cr>
nnoremap <leader>q :q<cr>
nnoremap <leader>x :x<cr>
nnoremap <leader>aq :qa<cr>
nnoremap <leader>as :wa<cr>
nnoremap <leader>ax :xa<cr>
nnoremap <leader><leader>q :q!<cr>
nnoremap <leader><leader>aq :qa!<cr>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ## Moving between windows
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

nnoremap <c-h> <c-w>h
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-l> <c-w>l

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ## Move Line/Selection Up/Down
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Move Line
nnoremap K ddkP
nnoremap J ddp

" Move Visual Selection
vnoremap J :m '>+1<cr>gv=gv
vnoremap K :m '<-2<cr>gv=gv

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ## Dotfiles
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Quick access dotfiles
nnoremap <leader>vrc :e ~/.ideavimrc<cr>

" Manually source .vimrc
nnoremap <leader><leader>r :source ~/.ideavimrc<cr>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ## Windows
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Splitting
nnoremap <leader>- :split<cr>
nnoremap <leader>/ :vsplit<cr>

" Moving
map <leader>wn <Action>(NextTab)
map <leader>wp <Action>(PreviousTab)

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ## System Clipboard
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Normal Mode
nnoremap <leader>y "+y
nnoremap <leader>d "+d
nnoremap <leader>p "+p
nnoremap <leader>P "+P

" Visual Mode
vnoremap <leader>d "+d
vnoremap <leader>y "+y
vnoremap <leader>p "+p
vnoremap <leader>P "+P

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ## Code Formatting and Navigation
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

map <leader>F <Action>(ReformatCode)
map <leader>ep <Action>(GotoPreviousError)
map <leader>en <Action>(GotoNextError)
map <leader>to <Action>(GotoDeclaration)
map <leader>tf <Action>(FindUsages)
map <leader>rf <Action>(RenameElement)

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ## Other Mappings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Semi-colon instead of colon for commands
nnoremap ; :
vnoremap ; :

" Enter creates a new line in normal mode
nnoremap <cr> o<esc>

" Enable folding with the space bar
map <space> <Action>(CollapseBlock)

" Remove highlighted search results
nnoremap <leader>noh :noh<cr>

" Show registers
nnoremap <leader>rg :reg<cr>

" Make . to work with visually selected lines
vnoremap . :normal.<cr>

" Fuzzy find files by name
map <leader>ff <Action>(GotoFile)

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
