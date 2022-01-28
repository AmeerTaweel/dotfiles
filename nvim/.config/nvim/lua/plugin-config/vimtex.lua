require "globals"
local vim_utils = require "utils.vim"


-- If we are not using kitty

if variables.environment.TERM ~= "xterm-kitty" then
	variables.global.vimtex_view_method = "zathura"
	return
end

-- If we are using kitty

-- Vimtex should not do anything
-- Will handle viewing in a custom way
variables.global.vimtex_view_general_viewer = "true"
variables.global.vimtex_view_general_options  = ""

vim_utils.create_auto_group("VimtexKitty", {
	[[User VimtexEventInitPost VimtexCompile]],
	[[User VimtexEventCompileSuccess execute 'lua require("plugin-config.vimtex").view()']],
	[[User VimtexEventView execute 'lua require("plugin-config.vimtex").view()']]
})

-- variables.global.vimtex_view_method = "zathura"
-- [ "$(kitty @ ls | grep -c '"title": "vimtex-live-output"')" -eq 0 ] && kitty @ --to "$KITTY_LISTEN_ON" new-window --keep-focus --title "vimtex-live-output" "$SHELL"; kitty @ --to "$KITTY_LISTEN_ON" send-text --match title:"vimtex-live-output" "echo test\n"
-- [ "$(kitty @ ls | grep -c '"title": "vimtex-live-output"')" -ne 0 ] && kitty @ --to "$KITTY_LISTEN_ON" send-text --match title:"vimtex-live-output" "\x12" || kitty @ --to "$KITTY_LISTEN_ON" new-window --keep-focus --title "vimtex-live-output" "$SHELL" && kitty @ --to "$KITTY_LISTEN_ON" send-text --match title:"vimtex-live-output" "termpdf.py
-- [ "$(kitty @ ls | grep -c '"title": "vimtex-live-output"')" -ne 0 ] && kitty @ --to "$KITTY_LISTEN_ON" send-text --match title:"vimtex-live-output" "\x12" || kitty @ --to "$KITTY_LISTEN_ON" new-window --keep-focus --title "vimtex-live-output" "$SHELL" && kitty @ --to "$KITTY_LISTEN_ON" send-text --match title:"vimtex-live-output" "termpdf.py @pdf"

-- local script = '[ "$(kitty @ ls | grep -c \'"title": "vimtex-live-output"\')" -ne 0 ]; '
-- script = script .. 'if [ "$?" -eq 0 ]; '
-- script = script .. 'then kitty @ --to "$KITTY_LISTEN_ON" send-text --match title:"vimtex-live-output" "\\x12"; '
-- script = script .. 'else kitty @ --to "$KITTY_LISTEN_ON" new-window --keep-focus --title "vimtex-live-output" "$SHELL"; '
-- script = script .. 'kitty @ --to "$KITTY_LISTEN_ON" send-text --match title:"vimtex-live-output" "termpdf.py '

local M = {}

M.view = function()
	local is_output_window_open = fn.system([[kitty @ ls | grep -c '"title": "vimtex-live-output"']]):gsub("\n", "")
	is_output_window_open = tonumber(is_output_window_open)

	local tex_file_path = fn.expand("%:p")
	local pdf_file_path = tex_file_path:gsub("tex$", "pdf")
	local out_file_path = tex_file_path:gsub("tex$", "out")

	if is_output_window_open == 0 then
		os.execute([[kitty @ --to "$KITTY_LISTEN_ON" new-window --keep-focus --title "vimtex-live-output" "$SHELL"]])
		os.execute([[kitty @ --to "$KITTY_LISTEN_ON" send-text --match title:"vimtex-live-output" "termpdf.py ]] .. pdf_file_path .. [[\\n"]])
	end

	local current_line = fn.line(".")
	local current_column = fn.col(".")

	local current_page = fn.system("synctex view -i " .. current_line .. ":" .. current_column .. ":" .. tex_file_path .. " -o " .. out_file_path .. " | grep -m 1 'Page'")
	current_page = current_page:gsub("Page:", "")
	current_page = current_page:gsub("\n", "")
	print(type(current_page), current_page)
	current_page = tonumber(current_page)

	-- os.execute([[kitty @ --to "$KITTY_LISTEN_ON" send-text --match title:"vimtex-live-output" "\\x12"]]) -- refresh termpdf.py
	-- fn.system([[kitty @ --to "$KITTY_LISTEN_ON" send-text --stdin --match title:"vimtex-live-output" "]] .. current_page .. [[G\\n"]]) -- go to current page
	local script = [[kitty @ --to "$KITTY_LISTEN_ON" send-text --match title:"vimtex-live-output" "]] .. current_page .. [["; ]]
	script = script .. [[sleep 0.05; kitty @ --to "$KITTY_LISTEN_ON" send-text --match title:"vimtex-live-output" "G"; ]] -- go to current page
	script = script .. [[sleep 0.05; kitty @ --to "$KITTY_LISTEN_ON" send-text --match title:"vimtex-live-output" "\\x12"]] -- go to current page
	os.execute(script) -- go to current page
	-- print([[kitty @ --to "$KITTY_LISTEN_ON" send-text --match title:"vimtex-live-output" "]] .. current_page .. [[G"]]) -- go to current page
end

return M
