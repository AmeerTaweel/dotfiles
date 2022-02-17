local todo = require "todo-comments"

todo.setup {
	signs = false,
	keywords = {
		FIX = {
			icon = " ", -- used for the sign and search results
			color = "error", -- hex or named color
			alt = { "FIXME", "BUG", "FIXIT", "ISSUE", "ERROR" }, -- alternatives
		},
		TODO = { icon = " ", color = "info" },
		WARN = { icon = " ", color = "warning", alt = { "WARNING" } },
		NOTE = { icon = " ", color = "hint", alt = { "INFO" } }
	},
	merge_keywords = false, -- merge custom keywords with defaults
	highlight = {
		keyword = "wide_end", -- "fg", "bg", "wide" , "wide_end", or empty.
	}
}
