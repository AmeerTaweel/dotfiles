local telescope = require "telescope"
local actions = require "telescope.actions"

-- Main Setup
telescope.setup {
	defaults = {
		mappings = {
			i = {
				["<esc>"] = actions.close,
				["<tab>"] = actions.move_selection_previous,
				["<s-tab>"] = actions.move_selection_next,
				["<c-s>"] = actions.toggle_selection
			}
		}
	}
}

-- Use the native fuzzy finder as it is faster
telescope.load_extension("fzf")

-- Load the ultisnips integration
telescope.load_extension("ultisnips")
