""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" # Conquer of Completion (COC) Configuration
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ## Plugins
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let g:coc_global_extensions = [
			\"coc-snippets",
			\"coc-browser",
			\"coc-calc",
			\"coc-highlight",
			\"coc-spell-checker",
			\"coc-diagnostic",
			\"coc-html",
			\"coc-htmlhint",
			\"coc-html-css-support",
			\"coc-css",
			\"coc-stylelint",
			\"coc-stylelintplus",
			\"coc-prettier",
			\"coc-yank",
			\"coc-eslint",
			\"coc-tsserver",
			\"coc-json",
			\"coc-clangd",
			\"coc-markdownlint",
			\"coc-pyright",
			\"coc-jedi",
			\"coc-pydocstring",
			\"coc-sh",
			\"coc-yaml",
			\"coc-vimlsp",
			\"coc-xml",
			\"coc-toml",
			\"coc-java"
			\]

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ## Settings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Highlight the symbol and its references when holding the cursor
autocmd CursorHold * silent call CocActionAsync("highlight")

augroup CoCSettings
	autocmd!
	" Show signature help on jump placeholder
	autocmd User CocJumpPlaceholder call CocActionAsync("showSignatureHelp")
augroup end

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ## Completion
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Enable TAB and SHIFT+TAB to cycle through completion suggestions
inoremap <silent><expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"
inoremap <silent><expr><s-tab> pumvisible() ? "\<c-p>" : "\<s-tab>"

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ## Navigation
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

nmap <silent> <leader>dp <plug>(coc-diagnostic-prev)
nmap <silent> <leader>dn <plug>(coc-diagnostic-next)
" NOTE: Use ":CocDiagnostics" to get all diagnostics of current buffer in location list

" GoTo code navigation
nmap <silent> gd <plug>(coc-definition)
nmap <silent> gy <plug>(coc-type-definition)
nmap <silent> gi <plug>(coc-implementation)
nmap <silent> gr <plug>(coc-references)

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ## Actions
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Symbol renaming
nmap <leader>rn <plug>(coc-rename)

" Format all code in the buffer
nnoremap <leader>fa :call CocAction("format")<cr>
" Formatting selected code
xmap <leader>fs <plug>(coc-format-selected)
nmap <leader>fs <plug>(coc-format-selected)

" Organize imports of the current buffer
nnoremap <leader>oi :call CocAction("runCommand", "editor.action.organizeImport")<cr>

" Show available code actions
nmap <leader>ac <plug>(coc-codeaction)
" Apply auto fix in the current line
nmap <leader>fx <plug>(coc-fix-current)
" Show available code actions for the selected region
" Example: "<leader>avap" for current paragraph
xmap <leader>av <plug>(coc-codeaction-selected)
nmap <leader>av <plug>(coc-codeaction-selected)

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

" Remap <C-f> and <C-b> to scroll long float windows/popups
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

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ## Text Objects
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" NOTE: Requires "textDocument.documentSymbol" support from the language server

" Map function text object
xmap if <plug>(coc-funcobj-i)
omap if <plug>(coc-funcobj-i)
xmap af <plug>(coc-funcobj-a)
omap af <plug>(coc-funcobj-a)

" Map class text object
xmap ic <plug>(coc-classobj-i)
omap ic <plug>(coc-classobj-i)
xmap ac <plug>(coc-classobj-a)
omap ac <plug>(coc-classobj-a)

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Use CTRL-S for selections ranges.
" Requires 'textDocument/selectionRange' support of language server.
nmap <silent> <C-s> <plug>(coc-range-select)
xmap <silent> <C-s> <plug>(coc-range-select)


" Add (Neo)Vim's native statusline support.
" NOTE: Please see `:h coc-status` for integrations with external plugins that
" provide custom statusline: lightline.vim, vim-airline.
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" Mappings for CoCList
" Show all diagnostics.
nnoremap <silent><nowait> <space>a  :<C-u>CocList diagnostics<cr>
" Manage extensions.
nnoremap <silent><nowait> <space>e  :<C-u>CocList extensions<cr>
" Show commands.
nnoremap <silent><nowait> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document.
nnoremap <silent><nowait> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols.
nnoremap <silent><nowait> <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent><nowait> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent><nowait> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list.
nnoremap <silent><nowait> <space>p  :<C-u>CocListResume<CR>

" append result on current expression
nmap <Leader>ca <plug>(coc-calc-result-append)
" replace result on current expression
nmap <Leader>cr <plug>(coc-calc-result-replace)

" Use <C-l> for trigger snippet expand.
imap <C-l> <plug>(coc-snippets-expand)

" Use <C-j> for select text for visual placeholder of snippet.
vmap <C-j> <plug>(coc-snippets-select)

" Use <C-j> for jump to next placeholder, it's default of coc.nvim
let g:coc_snippet_next = '<c-j>'

" Use <C-k> for jump to previous placeholder, it's default of coc.nvim
let g:coc_snippet_prev = '<c-k>'

" Use <C-j> for both expand and jump (make expand higher priority.)
imap <C-j> <plug>(coc-snippets-expand-jump)

" Use <leader>x for convert visual selected code to snippet
xmap <leader>x  <plug>(coc-convert-snippet)

autocmd CursorHold * silent call CocActionAsync('highlight')
