-- { Wallpaper }

-- Standard awesome library
local gears = require("gears")
-- Theme handling library
local beautiful = require("beautiful")

local M = {}

-- Sets the wallpaper for the screen
M.set = function(s)
	if beautiful.wallpaper then
		local wallpaper = beautiful.wallpaper
		-- If wallpaper is a function, call it with the screen
		if type(wallpaper) == "function" then
			wallpaper = wallpaper(s)
		end
		gears.wallpaper.maximized(wallpaper, s, true)
	end
end

return M
