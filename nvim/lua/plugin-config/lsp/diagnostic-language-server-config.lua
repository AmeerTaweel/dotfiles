--[[
+---------------------------------------------+
| # Diagnostics Language Server Configuration |
+---------------------------------------------+
--]]

require "globals"

local this = {}

this.filetypes = {
	"javascript",
	"typescript",
	"fish",
	"markdown",
	"python",
	"sh",
	"tex",
	"vim",
	"text",
	"lua"
}

this.init_options = {
	linters = {
		eslint = {
			command = "eslint",
			rootPatterns = {
				".git"
			},
			debounce = 100,
			args = {
				"--stdin",
				"--stdin-filename",
				"%filepath",
				"--format",
				"json"
			},
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
			securities = {
				["2"] = "error",
				["1"] = "warning"
			}
		},
		fish = {
			command = "fish",
			args = {"-n", "%file"},
			isStdout = false,
			isStderr = true,
			sourceName = "fish",
			formatLines = 1,
			formatPattern = {
				"^.*\\(line (\\d+)\\):(.*)$",
				{
					line = 1,
					message = 2
				}
			}
		},
		markdownlint = {
			command = "markdownlint",
			isStderr = true,
			debounce = 100,
			args = { "--stdin" },
			offsetLine = 0,
			offsetColumn = 0,
			sourceName = "markdownlint",
			formatLines = 1,
			formatPattern = {
				"^.*?:\\s?(\\d+)(:(\\d+)?)?\\s(MD\\d{3}\\/[A-Za-z0-9-/]+)\\s(.*?)\\[.*$",
				{
					line = 1,
					column = 3,
					message = {5, "[", 4, "]"}
				}
			}
		},
		pylint = {
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
			rootPatterns = {".git", "pyproject.toml", "setup.py"},
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
		},
		shellcheck = {
			command = "shellcheck",
			debounce = 100,
			args = {
				"--format",
				"json",
				"-"
			},
			sourceName = "shellcheck",
			parseJson = {
				line = "line",
				column = "column",
				endLine = "endLine",
				endColumn = "endColumn",
				message = "${message} [${code}]",
				security = "level"
			},
			securities = {
				error = "error",
				warning = "warning",
				info = "info",
				style = "hint"
			}
		},
		textidote = {
			command = "textidote",
			debounce = 100,
			args = {"--type", "tex", "--check", "en", "--output", "singleline", "--no-color"},
			offsetLine = 0,
			offsetColumn = 0,
			sourceName = "textidote",
			formatLines = 1,
			formatPattern = {
				"\\(L(\\d+)C(\\d+)-L(\\d+)C(\\d+)\\):(.+)\".+\"$",
				{
					line = 1,
					column = 2,
					endLine = 3,
					endColumn = 4,
					message = 5
				}
			}
		},
		vint = {
			command = "vint",
			debounce = 100,
			args = { "-s", "--enable-neovim", "%file"},
			offsetLine = 0,
			offsetColumn = 0,
			sourceName = "vint",
			formatLines = 1,
			formatPattern = {
				"[^:]+:(\\d+):(\\d+):\\s*(.*)(\\r|\\n)*$",
				{
					line = 1,
					column = 2,
					message = 3
				}
			}
		},
		["write-good"] = {
			command = "write-good",
			debounce = 100,
			args = { "--text=%text" },
			offsetLine = 0,
			offsetColumn = 1,
			sourceName = "write-good",
			formatLines = 1,
			formatPattern = {
				"(.*)\\s+on\\s+line\\s+(\\d+)\\s+at\\s+column\\s+(\\d+)\\s*$",
				{
					line = 2,
					column = 3,
					message = 1
				}
			}
		}
	},
	filetypes = {
		javascript = "eslint",
		typescript = "eslint",
		fish = "fish",
		markdown = {"markdownlint", "write-good"},
		python = "pylint",
		sh = "shellcheck",
		tex = "textidote",
		vim = "vint",
		text = "write-good"
	},
	formatters = {
		fish_indent = {
			command = "fish_indent"
		},
		prettier = {
			command = "prettier",
			args = {"--stdin-filepath", "%filepath"},
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
		},
		black = {
			command = "black",
			args = {"--quiet", "-"}
		},
		isort = {
			command = "isort",
			args = {"--quiet", "-"}
		},
		["lua-format"] = {
			command = "lua-format",
			args = {"-i"}
		}
	},
	formatFiletypes = {
		fish = "fish_indent",
		javascript = "prettier",
		typescript = "prettier",
		python = {"black", "isort"},
		lua = "lua-format"
	}
}

return this
