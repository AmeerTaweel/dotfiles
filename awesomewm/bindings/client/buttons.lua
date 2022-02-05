-- { Client Mouse Bindings }

local awful = require("awful")
local gears = require("gears")
local vars = require("user-variables")

local M = gears.table.join(
	-- Focus window on left-click
	awful.button({ }, 1, function(c)
		c:emit_signal("request::activate", "mouse_click", { raise = true })
	end),
	-- Move window by dragging while holding mod-key and left-click
	awful.button({ vars.mod_key }, 1, function(c)
		c:emit_signal("request::activate", "mouse_click", { raise = true })
		awful.mouse.client.move(c)
	end),
	-- Resize window by dragging while holding mod-key and right-click
	awful.button({ vars.mod_key }, 3, function(c)
		c:emit_signal("request::activate", "mouse_click", { raise = true })
		awful.mouse.client.resize(c)
	end)
)

return M
