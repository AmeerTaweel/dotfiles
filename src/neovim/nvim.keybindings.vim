""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" # Key-Bindings
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
vnoremap J :m '>+1<CR>gv=gv
vnoremap K :m '<-2<CR>gv=gv

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
" ## Dotfiles
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Manually source init.vim
nnoremap <leader><leader>r :source ~/.config/nvim/init.vim<cr>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ## File Explorer
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Manually source init.vim
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
" ## Zooming
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

fu! ToggleZoom()
	" Initialize the zoom state
	if !exists("g:isZoomed")
		let g:isZoomed = 0
	endif

	" Toggle zoom via a new tab
	if g:isZoomed
		let g:isZoomed = 0
		execute "tabclose"
	else
		let g:isZoomed = 1
		execute "tabnew %"
	endif
endfunction

" Toggle zoom
nnoremap <leader>tz :call ToggleZoom()<cr>

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
" ## Git
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Fugitive
nnoremap <leader>gac :Git add %<cr>
nnoremap <leader>gaa :Git add .<cr>
nnoremap <leader>gc :Git commit<cr>
nnoremap <leader>gps :Git push<cr>
nnoremap <leader>gpl :Git pull<cr>
nnoremap <leader>glg :Git log<cr>
nnoremap <leader>gst :Git status<cr>

" GitGutter
" ]c -> Next Hunk
" [c -> Previous Hunk
" <leader>hs -> Stage Hunk
" <leader>hu -> Undo Hunk
" ic -> In Hunk
" ac -> Around Hunk

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" Code Formatting and Navigation
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ALE Mappings
"" Fix code with ALE
nnoremap <leader>fc :ALEFix<cr>
"" Move to next and previous error with ALE
nnoremap <silent> <leader>ep :ALEPrevious<cr>
nnoremap <silent> <leader>en :ALENext<cr>

" YouCompleteMe Mappings
nnoremap <leader>to :YcmCompleter GoTo<cr>
nnoremap <leader>tf :YcmCompleter GoToReferences<cr>
nnoremap <leader>doc :YcmCompleter GetDoc<cr>
nnoremap <leader>rf :YcmCompleter RefactorRename<space>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" Quickfix list navigation
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Move to next location
nnoremap <leader>cn :cn<cr>
" Move to previous location
nnoremap <leader>cp :cp<cr>
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" Tabs
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
nnoremap <leader>tn  :tabnew<cr>
nnoremap <leader>tl  :tablast<cr>
nnoremap <leader>tc  :tabclose<cr>
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

" Fuzzy Finder Mappings
nnoremap <leader>ff :Files<cr>

" Window Swap: Swap Window
nnoremap <leader>ww :call WindowSwap#EasyWindowSwap()<cr>

" Find files using Telescope command-line sugar.
nnoremap <leader>ff <cmd>Telescope find_files<cr>
nnoremap <leader>fg <cmd>Telescope live_grep<cr>
nnoremap <leader>fb <cmd>Telescope buffers<cr>
nnoremap <leader>fh <cmd>Telescope help_tags<cr>
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
