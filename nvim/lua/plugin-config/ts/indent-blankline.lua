--[[
+--------------------+
| # Indent Blankline |
+--------------------+
--]]

local indentBlankLine = require "indent_blankline"

indentBlankLine.setup {
    buftype_exclude = {
		"terminal",
		"help"
	},
    filetype_exclude = {
		"NvimTree"
	},
	use_treesitter = true,
	show_current_context = true,
	context_patterns = {
		"class",
		"function",
		"method",
		"if_statement",
		"variable_declaration",
		"field"
	}
}
