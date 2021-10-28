--[[
+-----------------------+
| # TreeSitter Auto Tag |
+-----------------------+
--]]

local treesitter = require "nvim-treesitter.configs"

treesitter.setup {
	autotag = {
		enable = true
	}
}
