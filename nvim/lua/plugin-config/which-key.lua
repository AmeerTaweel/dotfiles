local whichKey = require "which-key"

whichKey.setup {
	plugins = {
		-- Show a list of marks on ' and `
		marks = true,
 		-- Show registers on " in NORMAL or <C-r> in INSERT mode
		registers = true,
		spelling = {
			-- Show WhichKey when pressing z= to select spelling suggestions
			enabled = true,
			-- Number of suggestions to be shown in the list
			suggestions = 20
		},
		-- The presets plugin adds help for a bunch of default keybindings in Neovim
		-- No actual key bindings are created
		presets = {
			operators = true,
			-- Add help for operators like d, y, ... and register them for motion / text object completion
			motions = true, -- Add help for motions
			text_objects = true, -- Help for text objects triggered after entering an operator
			windows = true, -- Default bindings on <c-w>
			nav = true, -- Misc bindings to work with windows
			z = true, -- Bindings for folds, spelling and others prefixed with z
			g = true -- Bindings for prefixed with g
		}
	},
	show_help = true, -- Show help message on the command line when the popup is visible
	triggers = "auto" -- Automatically setup triggers
}
