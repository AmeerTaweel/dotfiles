require "globals"

local package = "plugins.lsp.diagnostic-language-server"

local customize_server_options = function(config)
	config.filetypes = {
		-- "javascript",
		-- "typescript",
		-- "fish",
		"markdown",
		-- "python",
		"sh",
		"vim",
		-- "lua",
	}

	config.init_options = {
		linters = {
			eslint = require (package .. ".eslint"),
			fish = require (package .. ".fishlint"),
			markdownlint = require (package .. ".markdownlint"),
			pylint = require (package .. ".pylint"),
			shellcheck = require (package .. ".shellcheck"),
			vint = require (package .. ".vint")
		},
		filetypes = {
			-- javascript = "eslint",
			-- typescript = "eslint",
			-- fish = { "fish" },
			markdown = { "markdownlint" },
			-- python = { "pylint" },
			sh = { "shellcheck" },
			vim = { "vint" },
		},
		formatters = {
			fish_indent = require (package .. ".fishformat"),
			prettier = require (package .. ".prettier"),
			black = require (package .. ".black"),
			isort = require (package .. ".isort"),
			["lua-format"] = require (package .. ".lua-format")
		},
		formatFiletypes = {
			-- javascript = { "prettier" },
			-- typescript = { "prettier" },
			-- fish = { "fish_indent" },
			-- python = { "black", "isort" },
			-- lua = { "lua-format" }
		}
	}
end

return customize_server_options
