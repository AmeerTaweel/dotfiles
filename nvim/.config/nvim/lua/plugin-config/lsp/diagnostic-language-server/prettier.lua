local config = {
	command = "prettier",
	args = { "--stdin-filepath", "%filepath" },
	rootPatterns = {
		".prettierrc",
		".prettierrc.json",
		".prettierrc.toml",
		".prettierrc.json",
		".prettierrc.yml",
		".prettierrc.yaml",
		".prettierrc.json5",
		".prettierrc.js",
		".prettierrc.cjs",
		"prettier.config.js",
		"prettier.config.cjs"
	}
}

return config
