local awful = require("awful")

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
