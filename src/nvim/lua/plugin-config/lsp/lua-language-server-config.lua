--[[
+-------------------------------------+
| # Lua Language Server Configuration |
+-------------------------------------+
--]]

require "globals"

local this = {}

this.settings = {
	Lua = {
		runtime = {
			version = "LuaJIT",
			path = string.split(package.path, ";")
		},
		diagnostics = {
			-- Get the language server to recognize the "vim" global
			globals = {"vim"}
		},
		workspace = {
			-- Make the language server aware of Neovim's runtime files
			library = {
				[fn.expand("$VIMRUNTIME/lua")] = true,
				[fn.expand("$VIMRUNTIME/lua/vim/lsp")] = true
			}
		}
	}
}

return this
