-- { Client Keyboard Bindings }

local awful = require("awful")
local gears = require("gears")
local vars = require("user-variables")

local M = gears.table.join(
	awful.key(
		{ vars.mod_key }, "f",
		function (c)
			c.fullscreen = not c.fullscreen
			c:raise()
		end,
		{ description = "toggle fullscreen", group = "client" }
	),
	awful.key(
		{ vars.mod_key, "Shift" }, "c",
		function (c) c:kill() end,
		{ description = "close", group = "client" }
	),
	awful.key(
		{ vars.mod_key, "Shift" }, "f",
		awful.client.floating.toggle,
		{ description = "toggle floating", group = "client" }
	),
	awful.key(
		{ vars.mod_key, "Shift" }, "s",
		function (c) c:move_to_screen() end,
		{ description = "move to screen", group = "client" }
	)
)

return M
