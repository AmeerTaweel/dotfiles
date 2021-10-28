--[[
+----------------------------+
| # TreeSitter Configuration |
+----------------------------+
--]]

require "globals"

local treesitter = require "nvim-treesitter.configs"

treesitter.setup {
	ensure_installed = "all",
	highlight = {
		enable = true,
		additional_vim_regex_highlighting = false
	},
	indent = {
		enable = true
	}
}

options.window.foldmethod = "expr"
options.window.foldexpr = "nvim_treesitter#foldexpr()"
options.window.foldenable = false
