--[[
+--------------------+
| # Vim Event System |
+--------------------+
--]]

require "globals"
local utils = require "utils"

local this = {}

local eventIdPrefixes = {
	keymap = "<KEYMAP>",
	command = "<COMMAND>"
}
local eventListeners = {}

this.addEventListener = function(eventId, callback)
	if eventListeners[eventId] == nil then
		eventListeners[eventId] = {}
	end
	table.insert(eventListeners[eventId], callback)
end

this.broadcastEvent = function(eventId)
	for _, callback in ipairs(eventListeners[eventId]) do
		return callback()
	end
end

---Defines a keymap that calls a lua function when triggered
---@param mode string
---@param keys string
---@param callback function
---@param options? table
this.createKeyMap = function(mode, keys, callback, options)
	options = options or {} -- options is optional
	options.expr = true
	local eventId = eventIdPrefixes.keymap .. utils.replaceTermCodes (mode .. keys)
	this.addEventListener(eventId, callback)
	local vimScriptExpression = [[luaeval("require('vim-event-system').broadcastEvent(']] .. eventId .. [[')")]]
	setKeymap(mode, keys, vimScriptExpression, options)
end

---Defines a custom editor command that calls a lua function when triggered
---@param name string
---@param callback function
this.createCustomEditorCommand = function(name, callback)
	local eventId = eventIdPrefixes.command .. name
	this.addEventListener(eventId, callback)
	local vimScriptExpression = [['lua require("vim-event-system").broadcastEvent("]] .. eventId .. [[")']]
	exec.vimscript("command! " .. name .. " execute " .. vimScriptExpression)
end

return this
