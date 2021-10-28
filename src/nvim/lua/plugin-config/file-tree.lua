require "globals"

local treeConfig = require "nvim-tree.config"
local treeCallback = treeConfig.nvim_tree_callback

variables.global.nvim_tree_width = "15%"
variables.global.nvim_tree_ignore = {".git"}
variables.global.nvim_tree_auto_close = 1
variables.global.nvim_tree_follow = 1
variables.global.nvim_tree_quit_on_open = 0
variables.global.nvim_tree_git_hl = 1
variables.global.nvim_tree_highlight_opened_files = 1
variables.global.nvim_tree_disable_netrw = 0
variables.global.nvim_tree_hijack_netrw = 0
variables.global.nvim_tree_add_trailing = 1
variables.global.nvim_tree_lsp_diagnostics = 1
variables.global.nvim_tree_show_icons = {
	git = 1,
	folders = 1,
	files = 1,
	folder_arrows = 0
}
variables.global.nvim_tree_icons = {
	default = '',
	symlink = '',
	git = {
		unstaged = "✗",
		-- staged = "✓",
		unmerged = "",
		renamed = "➜",
		untracked = "★",
		deleted = "",
		ignored = "◌"
	},
	folder = {
		arrow_open = "",
		arrow_closed = "",
		default = "",
		open = "",
		empty = "",
		empty_open = "",
		symlink = "",
		symlink_open = ""
	},
	lsp = {
		hint = "",
		info = "",
		warning = "",
		error = ""
	}
}
