-- { Layouts }

local awful = require("awful")

local all_layouts = {
    awful.layout.suit.max,
    awful.layout.suit.max.fullscreen,
    awful.layout.suit.floating,
    awful.layout.suit.tile,
    awful.layout.suit.tile.left,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.tile.top,
    awful.layout.suit.fair,
    awful.layout.suit.fair.horizontal,
    awful.layout.suit.spiral,
    awful.layout.suit.spiral.dwindle,
    awful.layout.suit.magnifier,
    awful.layout.suit.corner.nw,
    awful.layout.suit.corner.ne,
    awful.layout.suit.corner.sw,
    awful.layout.suit.corner.se,
}

local M = {
    max = awful.layout.suit.max,
    fullscreen = awful.layout.suit.max.fullscreen,
    floating = awful.layout.suit.floating,
    tile = awful.layout.suit.tile,
}

-- Table of layouts to cover with awful.layout.inc (order matters)
M.list = {
	M.max,
	M.fullscreen,
	M.floating,
	M.tile
}

return M
