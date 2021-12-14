-- { Metatables Utilities }

local M = {}

-- Return a default value when accessing an undefined key
M.set_default_value = function(table, default_value)
	local metatable = getmetatable(table)
	-- Create an empty metatable if table does not already have one
	if metatable == nil then
		metatable = {}
	end
	metatable.__index = function(table, key)
		-- __index gets called only if the key is not already in the table
		return default_value
	end
	setmetatable(table, metatable)
end

return M
