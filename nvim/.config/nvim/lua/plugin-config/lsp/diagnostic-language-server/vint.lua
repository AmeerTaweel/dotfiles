local config = {
	command = "vint",
	debounce = 100,
	args = { "-s", "--enable-neovim", "%file" },
	offsetLine = 0,
	offsetColumn = 0,
	sourceName = "vint",
	formatLines = 1,
	formatPattern = {
		"[^:]+:(\\d+):(\\d+):\\s*(.*)(\\r|\\n)*$",
		{ line = 1, column = 2, message = 3 }
	}
}

return config
