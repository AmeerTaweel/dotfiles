--[[
+---------------+
| # Keybindings |
+---------------+
--]]


require "globals"
local utils = require "utils"

-- TODO: Git shortlog command
-- "map <F2> :!git shortlog -s -n %<cr>"

-- Leader Key
variables.global.mapleader = ","

-- +------------------------+
-- | ## Saving and Quitting |
-- +------------------------+

utils.nnoremap("<leader>s", ":w<cr>")
utils.nnoremap("<leader>q", ":q<cr>")
utils.nnoremap("<leader>x", ":x<cr>")
utils.nnoremap("<leader>aq", ":qa<cr>")
utils.nnoremap("<leader>as", ":wa<cr>")
utils.nnoremap("<leader>ax", ":xa<cr>")
utils.nnoremap("<leader><leader>q", ":q!<cr>")
utils.nnoremap("<leader><leader>aq", ":qa!<cr>")

-- +--------------------------------+
-- | ## Move Line/Selection Up/Down |
-- +--------------------------------+

-- Move Line
utils.nnoremap("K", "ddkP")
utils.nnoremap("J", "ddp")

-- Move Visual Selection
utils.vnoremap("J", ":m '>+1<CR>gv=gv")
utils.vnoremap("K", ":m '<-2<CR>gv=gv")

-- +----------------------+
-- | ## Splitting Windows |
-- +----------------------+

utils.nnoremap("<leader>-", ":split<cr>")
utils.nnoremap("<leader>/", ":vsplit<cr>")

-- Swap Window
utils.nnoremap("<leader>ww", ":call WindowSwap#EasyWindowSwap()<cr>")

--[[

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
nnoremap <silent><leader>z :ZoomWin<cr>
" The <c-u> in the command prevents the range error
" More info in this thread:
" https://vi.stackexchange.com/questions/7149/mapping-a-command-in-visual-mode-results-in-error-e481-no-range-alllowed
vnoremap <silent><leader>z :<c-u>ZoomWin<cr>gv

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
" <leader>hp -> Preview Hunk
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
" nnoremap <leader>to :YcmCompleter GoTo<cr>
" nnoremap <leader>tf :YcmCompleter GoToReferences<cr>
" nnoremap <leader>doc :YcmCompleter GetDoc<cr>
" nnoremap <leader>rf :YcmCompleter RefactorRename<space>
nnoremap <silent> <leader>gd <cmd>lua vim.lsp.buf.definition()<CR>
nnoremap <silent> <leader>gD <cmd>lua vim.lsp.buf.declaration()<CR>
nnoremap <silent> <leader>gr <cmd>lua vim.lsp.buf.references()<CR>
nnoremap <silent> <leader>gi <cmd>lua vim.lsp.buf.implementation()<CR>
nnoremap <leader>doc <cmd>lua vim.lsp.buf.hover()<cr>
nnoremap <silent> <C-k> <cmd>lua vim.lsp.buf.signature_help()<CR>
nnoremap <silent> <C-n> <cmd>lua vim.lsp.diagnostic.goto_prev()<CR>
nnoremap <silent> <C-p> <cmd>lua vim.lsp.diagnostic.goto_next()<CR>

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
" ## Tabs
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

nnoremap <leader>tn  :tabnew<cr>
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


" Find files using Telescope command-line sugar.
nnoremap <leader>ff <cmd>Telescope find_files<cr>
nnoremap <leader>fg <cmd>Telescope live_grep<cr>
nnoremap <leader>fb <cmd>Telescope buffers<cr>
nnoremap <leader>fh <cmd>Telescope help_tags<cr>
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
nnoremap <silent><nowait> <leader>da  :<c-u>CocList diagnostics<cr>
" Find symbol of current document
nnoremap <silent><nowait> <leader>bs  :<c-u>CocList outline<cr>
" Search project symbols
nnoremap <silent><nowait> <leader>ps  :<c-u>CocList -I symbols<cr>
" Yanked Text List
nnoremap <silent><nowait> <leader>yl  :<c-u>CocList yank<cr>
" Resume latest coc list
nnoremap <silent><nowait> <leader>rl  :<c-u>CocListResume<cr>

" ### Math

" Replace result on current expression
nmap <leader>mr <plug>(coc-calc-result-replace)

" ### Snippets

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
--]]
