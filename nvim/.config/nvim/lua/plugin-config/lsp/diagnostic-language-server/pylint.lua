local config = {
	sourceName = "pylint",
	command = "pylint",
	args = {
		"--output-format",
		"text",
		"--score",
		"no",
		"--msg-template",
		"'{line}:{column}:{category}:{msg} ({msg_id}:{symbol})'",
		"%file"
	},
	formatPattern = {
		"^(\\d+?):(\\d+?):([a-z]+?):(.*)$",
		{
			line = 1,
			column = 2,
			security = 3,
			message = 4
		}
	},
	rootPatterns = { ".git", "pyproject.toml", "setup.py" },
	securities = {
		informational = "hint",
		refactor = "info",
		convention = "info",
		warning = "warning",
		error = "error",
		fatal = "error"
	},
	offsetColumn = 1,
	formatLines = 1
}

return config
