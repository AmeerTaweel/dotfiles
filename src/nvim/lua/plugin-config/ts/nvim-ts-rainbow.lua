--[[
+------------------------------------+
| # TreeSitter Rainbow Configuration |
+------------------------------------+
--]]

local treesitter = require "nvim-treesitter.configs"

treesitter.setup {
	rainbow = {
		enable = true,
		-- Highlight also non-parentheses delimiters (boolean or table)
		extended_mode = true,
		-- Do not enable for files with more than x lines (int)
		max_file_lines = 1000
	}
}
