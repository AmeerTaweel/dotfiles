require "globals"

local M = {}

M.create_auto_command = function(autoCommand)
	exec.vimscript ("autocmd " .. autoCommand)
end

M.create_auto_group = function(name, autoCommands, isLocalToBuffer)
	isLocalToBuffer = isLocalToBuffer or false
	exec.vimscript ("augroup " .. name)

	-- Clear previous auto groups with this name
	if isLocalToBuffer then
		exec.vimscript "autocmd! * <buffer>"
	else
		exec.vimscript "autocmd!"
	end

	for _, autoCommand in ipairs(autoCommands) do
		M.create_auto_command(autoCommand)
	end
	exec.vimscript "augroup end"
end

return M
