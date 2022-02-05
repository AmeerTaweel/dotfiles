-- { Global Mouse Bindings }

local awful = require("awful")
local gears = require("gears")

local M = gears.table.join(
	-- Move to the next tag
    awful.button({ }, 4, awful.tag.viewnext),
	-- Move to the previous tag
    awful.button({ }, 5, awful.tag.viewprev)
)

return M
