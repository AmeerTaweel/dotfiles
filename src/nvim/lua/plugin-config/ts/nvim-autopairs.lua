--[[
+--------------+
| # Auto Pairs |
+--------------+
--]]

local autoPairs = require "nvim-autopairs"
local nvimCompeIntegration = require "nvim-autopairs.completion.compe"
local treesitter = require "nvim-treesitter.configs"

autoPairs.setup{
	disable_filetype = { "TelescopePrompt" },
    ts_config = { -- Enable using TreeSitter
		check_ts = true
    }
}

nvimCompeIntegration.setup({
  map_cr = true -- Map <cr> on insert mode
})

treesitter.setup {
	autopairs = {
		enable = true
	}
}
