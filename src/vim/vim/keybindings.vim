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
" ## Dotfiles
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
nnoremap <leader>sx :Sex<cr>
nnoremap <leader>vx :Vex<cr>

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

" NOTE: Vim needs to be compiled with the +clipboard flag for this to work.

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
nnoremap <leader>tc  :tabclose<cr>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ## Zooming
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

nnoremap <silent><leader>z :ZoomWin<cr>
" The <c-u> in the command prevents the range error
" More info in this thread:
" https://vi.stackexchange.com/questions/7149/mapping-a-command-in-visual-mode-results-in-error-e481-no-range-alllowed
vnoremap <silent><leader>z :<c-u>ZoomWin<cr>gv

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ## Fuzzy Find
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Find files by name
nnoremap <leader>ff :Files<cr>
" Search current buffer content
nnoremap <leader>fc :BLines<cr>
" Search buffers by name
nnoremap <leader>fb :Buffers<cr>
" Search content of open buffers
nnoremap <leader>fl :Lines<cr>
" Search in project files content
nnoremap <leader>fp :Rg<cr>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ## Git and Version Control
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" ### Vim Fugitive

nnoremap <leader>gac :Git add %<cr>
nnoremap <leader>gaa :Git add .<cr>
nnoremap <leader>gc :Git commit<cr>
nnoremap <leader>gps :Git push<cr>
nnoremap <leader>gpl :Git pull<cr>
nnoremap <leader>glg :Git log<cr>
nnoremap <leader>gst :Git status<cr>
" Opens git status with the ability to stage and unstage commits
"	s -> stage
"	u -> unstage
"	= -> see diff
"	cc -> commit
nnoremap <leader>gm :G<cr>

" ### CoC Git

" Next Hunk
nmap <leader>hn <plug>(coc-git-nextchunk)
" Previous Hunk
nmap <leader>hp <plug>(coc-git-prevchunk)
" Preview Hunk Info
nnoremap <silent> <leader>hi :CocCommand git.chunkInfo<cr>
" Stage Hunk
nnoremap <silent> <leader>hs :CocCommand git.chunkStage<cr>
" Undo Hunk
nnoremap <silent> <leader>hu :CocCommand git.chunkUndo<cr>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ## Code Formatting and Navigation
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" ### Navigation

nmap <silent> <leader>dp <plug>(coc-diagnostic-prev)
nmap <silent> <leader>dn <plug>(coc-diagnostic-next)

" GoTo code navigation
nmap <silent> gd <plug>(coc-definition)
nmap <silent> gy <plug>(coc-type-definition)
nmap <silent> gi <plug>(coc-implementation)
nmap <silent> gr <plug>(coc-references)

" ### Actions

" Symbol renaming
nmap <leader>rn <plug>(coc-rename)

" Format all code in the buffer
nnoremap <leader>fa :call CocAction("format")<cr>
" Formatting selected code
xmap <leader>fs <plug>(coc-format-selected)
nmap <leader>fs <plug>(coc-format-selected)

" Organize imports of the current buffer
nnoremap <leader>oi :CocCommand editor.action.organizeImport<cr>

" Show available code actions
nmap <leader>ac <plug>(coc-codeaction)
" Apply auto fix in the current line
nmap <leader>fx <plug>(coc-fix-current)

" Show documentation in a preview window
nnoremap <silent> <leader>ds :call <sid>showDocumentation()<cr>

function! s:showDocumentation()
	if (index(["vim","help"], &filetype) >= 0)
		execute "h ".expand("<cword>")
	elseif (coc#rpc#ready())
		call CocActionAsync("doHover")
	else
		execute "!" . &keywordprg . " " . expand("<cword>")
	endif
endfunction

" Remap <c-f> and <c-b> to scroll long float windows/popups
if has("nvim-0.4.0") || has("patch-8.2.0750")
	" Remap <c-f>
	nnoremap <silent><nowait><expr> <c-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<c-f>"
	inoremap <silent><nowait><expr> <c-f> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1)\<cr>" : "\<right>"
	vnoremap <silent><nowait><expr> <c-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<c-f>"
	" Remap <c-b>
	nnoremap <silent><nowait><expr> <c-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<c-b>"
	inoremap <silent><nowait><expr> <c-b> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0)\<cr>" : "\<left>"
	vnoremap <silent><nowait><expr> <c-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<c-b>"
endif

" ### CoCList

" Show all diagnostics
nnoremap <silent><nowait> <leader>da  :<C-u>CocList diagnostics<cr>
" Find symbol of current document
nnoremap <silent><nowait> <leader>bs  :<C-u>CocList outline<cr>
" Search project symbols
nnoremap <silent><nowait> <leader>ps  :<C-u>CocList -I symbols<cr>
" Resume latest coc list
nnoremap <silent><nowait> <leader>rl  :<C-u>CocListResume<CR>

" ### Math

" Replace result on current expression
nmap <leader>mr <plug>(coc-calc-result-replace)

" ### Snippets

" Use <c-j> for both expand and jump (make expand higher priority.)
imap <c-j> <plug>(coc-snippets-expand-jump)
" Use <c-j> for select text for visual placeholder of snippet.
vmap <c-j> <plug>(coc-snippets-select)
" Use <c-j> for jump to next placeholder, it's default of coc.nvim
let g:coc_snippet_next = "<c-j>"
" Use <c-k> for jump to previous placeholder, it's default of coc.nvim
let g:coc_snippet_prev = "<c-k>"

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
" ## Other Mappings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Enter creates a new line in normal mode
nnoremap <cr> o<esc>

" Go back in the last t, f, T, and F using space
nnoremap <space> ,

" Remove highlighted search results
nnoremap <leader>noh :noh<cr>

" Show registers
nnoremap <leader>rg :reg<cr>

" Make . to work with visually selected lines
vnoremap . :normal.<cr>

" Window Swap: Swap Window
nnoremap <leader>ww :call WindowSwap#EasyWindowSwap()<cr>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
