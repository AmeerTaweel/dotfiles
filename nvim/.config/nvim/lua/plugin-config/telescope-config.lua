local telescope = require "telescope"
local actions = require "telescope.actions"

-- Main Setup
telescope.setup {
	defaults = {
		mappings = {
			i = {
				["<esc>"] = actions.close
			}
		}
	}
}

-- Use the native fuzzy finder as it is faster
telescope.load_extension("fzf")
