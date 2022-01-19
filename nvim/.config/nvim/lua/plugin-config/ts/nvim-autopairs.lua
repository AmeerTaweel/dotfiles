local auto_pairs = require "nvim-autopairs"
local nvim_compe_integration = require "nvim-autopairs.completion.compe"
local treesitter = require "nvim-treesitter.configs"

auto_pairs.setup{
	disable_filetype = { "TelescopePrompt" },
	enable_check_bracket_line = true,  -- Don't add closing pair if it exists
	disable_in_macro = true,
	disable_in_visualblock = true,
    ts_config = { -- Enable using TreeSitter
		check_ts = true
    }
}

nvim_compe_integration.setup({
	map_cr = true -- Map <cr> on insert mode
})

treesitter.setup {
	autopairs = {
		enable = true
	}
}
