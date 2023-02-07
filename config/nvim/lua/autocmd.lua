require "globals"

-- { Custom Filetypes }

vim.filetype.add({
	extension = {
		tmux = "tmux"
	},
	pattern = {},
	filename = {
		[".tmux.conf"] = "tmux",
	}
})

-- { Auto Commands }

local create_auto_group = vim.api.nvim_create_augroup
local create_auto_command = vim.api.nvim_create_autocmd

-- Automatically re-balance windows on resize
create_auto_command("VimResized", {
	group =  create_auto_group("AUTO_REBALANCE", { clear = true }),
	pattern = "*",
	command = "wincmd ="
})

-- Disable auto-commenting
create_auto_command("FileType", {
	group =  create_auto_group("DISABLE_AUTO_COMMENT", { clear = true }),
	pattern = "*",
	command = "setlocal formatoptions-=c formatoptions-=r formatoptions-=o"
})

-- Make line numbers absolute when in insert mode and on buffer leaving
local smart_line_numbers = create_auto_group("SMART_LINE_NUMBERS", { clear = true })
create_auto_command({ "BufEnter", "FocusGained", "InsertLeave" }, {
	group = smart_line_numbers,
	pattern = "*",
	command = "setlocal relativenumber"
})
create_auto_command({ "BufLeave", "FocusLost", "InsertEnter" }, {
	group = smart_line_numbers,
	pattern = "*",
	command = "setlocal norelativenumber"
})

-- Disable whitespace character hints in netrw
create_auto_command("FileType", {
	group =  create_auto_group("DISABLE_NETRW_LIST", { clear = true }),
	pattern = "netrw",
	command = "setlocal nolist"
})
