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
" ## Follow Symlinks
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

fu! FollowSymlink(symlinkPath)
	let fullPath = resolve(expand(a:symlinkPath))
	execute "edit " . fullPath
endfunction

nnoremap gs :call FollowSymlink('<cfile>:p')<cr>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" # Dotfiles
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Quick access dotfiles
nnoremap <leader>vrc :call FollowSymlink($MYVIMRC)<cr>

" Manually source .vimrc
nnoremap <leader><leader>r :source $MYVIMRC<cr>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ## File Explorer
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

nnoremap <leader>ex :Ex<cr>
nnoremap <leader>sex :Sex<cr>
nnoremap <leader>vex :Vex<cr>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ## Splitting windows
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

nnoremap <leader>- :split<cr>
nnoremap <leader>/ :vsplit<cr>

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
" ## Tabs
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

nnoremap <leader>tn  :tabnew<cr>
nnoremap <leader>tl  :tablast<cr>
nnoremap <leader>tc  :tabclose<cr>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ## Zooming
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

nnoremap <silent><leader>z :MaximizerToggle<cr>
vnoremap <silent><leader>z :MaximizerToggle<cr>gv
inoremap <silent><leader>z <c-o>:MaximizerToggle<cr>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ## Git and Version Control
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Vim Fugitive
nnoremap <leader>gac :Git add %<cr>
nnoremap <leader>gaa :Git add .<cr>
nnoremap <leader>gc :Git commit<cr>
nnoremap <leader>gps :Git push<cr>
nnoremap <leader>gpl :Git pull<cr>
nnoremap <leader>glg :Git log<cr>
nnoremap <leader>gst :Git status<cr>
"" Opens git status with the ability to stage and unstage commits
""	s -> stage
""	u -> unstage
""	= -> see diff
""	cc -> commit
nnoremap <leader>gm :G<cr>

" Vim GitGutter
" ]c -> Next Hunk
" [c -> Previous Hunk
" <leader>hs -> Stage Hunk
" <leader>hu -> Undo Hunk
" <leader>hp -> Preview Hunk
" ic -> In Hunk
" ac -> Around Hunk

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ## Code Formatting and Navigation
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" ALE Mappings
"" Fix code with ALE
nnoremap <leader>F :ALEFix<cr>
"" Move to next and previous error with ALE
nnoremap <silent> <leader>ep :ALEPrevious<cr>
nnoremap <silent> <leader>en :ALENext<cr>

" YouCompleteMe Mappings
nnoremap <leader>to :YcmCompleter GoTo<cr>
nnoremap <leader>tf :YcmCompleter GoToReferences<cr>
nmap <c-k> <plug>(YCMHover)
nnoremap <leader>doc :YcmCompleter GetDoc<cr>
nnoremap <leader>rf :YcmCompleter RefactorRename<space>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ## Quickfix list navigation
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Move to next location
nnoremap <leader>cn :cn<cr>
" Move to previous location
nnoremap <leader>cp :cp<cr>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ## Fuzzy Find
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Find files by name
nnoremap <leader>ff :Files<cr>
" Grep content
nnoremap <leader>fg :Rg<cr>
" Search files tracked by git
nnoremap <leader>gf :GitFiles<cr>
" Search buffers
nnoremap <leader>fb :Buffers<cr>
" Search buffers content
nnoremap <leader>fl :Lines<cr>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" Other Mappings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Semi-colon instead of colon for commands
nnoremap ; :
vnoremap ; :

" Enter creates a new line in normal mode
nnoremap <cr> o<esc>

" Enable folding with the space bar
nnoremap <space> za

" Remove highlighted search results
nnoremap <leader>noh :noh<cr>

" Show registers
nnoremap <leader>rg :reg<cr>

" Make . to work with visually selected lines
vnoremap . :normal.<cr>

" Toggle spell checking
nnoremap <leader>ts :set spell! spelllang=en_us<cr>

" Window Swap: Swap Window
nnoremap <leader>ww :call WindowSwap#EasyWindowSwap()<cr>
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
