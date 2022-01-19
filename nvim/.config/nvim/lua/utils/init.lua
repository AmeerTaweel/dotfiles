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

return module
