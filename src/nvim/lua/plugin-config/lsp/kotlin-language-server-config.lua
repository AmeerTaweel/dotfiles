--[[
+----------------------------------------+
| # Kotlin Language Server Configuration |
+----------------------------------------+
--]]

require "globals"
local lspConfigUtils = require "lspconfig/util"

local this = {}

this.root_dir = lspConfigUtils.root_pattern("settings.gradle", "settings.gradle.kts")

this.settings = {
	kotlin = {
		debounceTime = 10,
		linting = {
			debounceTime = 10
		}
	}
}

return this
