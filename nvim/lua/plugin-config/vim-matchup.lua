require "globals"

local treesitter = require "nvim-treesitter.configs"

treesitter.setup {
	matchup = {
		enable = true,
		include_match_words = true
	}
}

-- Turn off offscreen highlighting
variables.global.matchup_matchparen_offscreen = {}
