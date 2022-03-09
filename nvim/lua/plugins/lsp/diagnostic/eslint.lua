local config = {
	command = "eslint",
	rootPatterns = { ".git" },
	debounce = 100,
	args = { "--stdin", "--stdin-filename", "%filepath", "--format", "json" },
	sourceName = "eslint",
	parseJson = {
		errorsRoot = "[0].messages",
		line = "line",
		column = "column",
		endLine = "endLine",
		endColumn = "endColumn",
		message = "${message} [${ruleId}]",
		security = "severity"
	},
	securities = { ["1"] = "warning", ["2"] = "error" }
}

return config
