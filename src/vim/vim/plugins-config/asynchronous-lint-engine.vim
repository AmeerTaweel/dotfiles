""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" # ALE Configuration
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" ALE message format
let g:ale_echo_msg_format = "[%linter%] %s [%severity%]"

" Automatically fix files on save
let g:ale_fix_on_save = 1

" Define fixers for different languages
let b:ale_fixers = {
	\ "*": ["remove_trailing_lines", "trim_whitespace"],
	\ "python": ["autoimport", "isort", "autopep8", "black"],
	\ "cpp": ["astyle", "clangtidy", "uncrustify", "clang-format"],
	\ "javascript": ["prettier"],
	\ "css": ["prettier"],
	\}
