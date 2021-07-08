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
			\"coc-java",
			\"coc-kotlin"
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

" Enable CTRL+j to confirm selection
inoremap <expr> <c-j> pumvisible() ? "\<c-y>" : "\<c-g>u\<cr>"

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

