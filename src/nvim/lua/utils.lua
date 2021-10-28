require "globals"

local module = {}

module.createAutoCommand = function(autoCommand)
	exec.vimscript ("autocmd " .. autoCommand)
end

module.createAutoGroup = function(name, autoCommands, isLocalToBuffer)
	isLocalToBuffer = isLocalToBuffer or false
	exec.vimscript ("augroup " .. name)

	-- Clear previous auto groups with this name
	if isLocalToBuffer then
		exec.vimscript "autocmd! * <buffer>"
	else
		exec.vimscript "autocmd!"
	end

	for _, autoCommand in ipairs(autoCommands) do
		module.createAutoCommand(autoCommand)
	end
	exec.vimscript "augroup end"
end

module.replaceTermCodes = function(keys)
	return replaceTermCodes(keys, true, true, true)
end

module.defineNoRemapKeybinding = function(mode, keys, action)
	local options = { noremap = true }
	setKeymap(mode, keys, action, options)
end

module.nnoremap = function(keys, action)
	module.defineNoRemapKeybinding("n", keys, action)
end

module.vnoremap = function(keys, action)
	module.defineNoRemapKeybinding("v", keys, action)
end

return module
