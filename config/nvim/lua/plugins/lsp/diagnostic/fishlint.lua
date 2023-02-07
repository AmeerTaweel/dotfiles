local config = {
	command = "fish",
	args = { "-n", "%file" },
	isStdout = false,
	isStderr = true,
	sourceName = "fish",
	formatLines = 1,
	formatPattern = { "^.*\\(line (\\d+)\\):(.*)$", { line = 1, message = 2 } }
}

return config
