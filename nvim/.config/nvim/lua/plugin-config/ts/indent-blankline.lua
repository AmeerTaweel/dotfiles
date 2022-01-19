local indent_blank_line = require "indent_blankline"

indent_blank_line.setup {
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
