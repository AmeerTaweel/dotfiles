--[[
+---------------------------------------+
| # Compe Auto-Completion Configuration |
+---------------------------------------+
--]]

require "globals"
local compe = require "compe"
local utils = require "utils"
local vimEventSystem = require "vim-event-system"

local this = {}

-- +----------+
-- | ## Setup |
-- +----------+

compe.setup {
	enabled = true,
	autocomplete = true,
	debug = false,
	min_length = 1,
	preselect = "enable",
	throttle_time = 40, -- Default: 80
	source_timeout = 200,
	resolve_timeout = 800,
	incomplete_delay = 400,
	max_abbr_width = 100,
	max_kind_width = 100,
	max_menu_width = 100,
	documentation = {
		border = { '', '' ,'', ' ', '', '', '', ' ' }, -- the border option is the same as `|help nvim_open_win|`
		winhighlight = "NormalFloat:CompeDocumentation,FloatBorder:CompeDocumentationBorder",
		max_width = 120,
		min_width = 60,
		max_height = math.floor(options.global.lines * 0.3),
		min_height = 1
	},

	source = {
		path = true,
		buffer = true,
		calc = true,
		nvim_lsp = true,
		nvim_lua = true,
		vsnip = true,
		ultisnips = true,
		luasnip = true,
		spell = true,
		tags = true,
		emoji = true,
		treesitter = true
	}
}

-- +-------------------------------------+
-- | ## Navigating Completions With Tabs |
-- +-------------------------------------+

--[[
# Use tab and shift-tab to:
* Move to next/previous item in the completion
* Jump to next/previous placeholder in snippet
--]]

this.tabCompletion = function()
	if fn.pumvisible() == 1 then -- If completion menu is visible
		return utils.replaceTermCodes(key.ctrlN) -- Navigate to next item
	else
		return utils.replaceTermCodes(key.tab) -- Act as a normal tab
	end
end

this.shiftTabCompletion = function()
	if fn.pumvisible() == 1 then -- If completion menu is visible
		return utils.replaceTermCodes(key.ctrlP) -- Navigate to previous item
	else
		return utils.replaceTermCodes(key.shiftTab) -- Act as a normal shift-tab
	end
end

vimEventSystem.createKeyMap(mode.insert, key.tab, this.tabCompletion)
vimEventSystem.createKeyMap(mode.insert, key.shiftTab, this.shiftTabCompletion)
vimEventSystem.createKeyMap(mode.select, key.tab, this.tabCompletion)
vimEventSystem.createKeyMap(mode.select, key.shiftTab, this.shiftTabCompletion)

return this
