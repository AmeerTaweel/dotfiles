--[[
+---------------------------+
| # Telescope Configuration |
+---------------------------+
--]]

local telescope = require "telescope"
local actions = require "telescope.actions"

-- Main Setup
telescope.setup {
	defaults = {
		file_sorter = require('telescope.sorters').get_fzy_sorter,
		prompt_prefix = ' >',
		color_devicons = true,

		file_previewer   = require('telescope.previewers').vim_buffer_cat.new,
		grep_previewer   = require('telescope.previewers').vim_buffer_vimgrep.new,
		qflist_previewer = require('telescope.previewers').vim_buffer_qflist.new,
	},
	extensions = {
		fzy_native = {
			override_generic_sorter = false,
			override_file_sorter = true,
		}
	}
}

-- Use the native fuzzy finder as it is faster
telescope.load_extension("fzy_native")
