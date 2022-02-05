local awful = require("awful")
local beautiful = require("beautiful")
local wallpaper = require("ui.wallpaper")

-- { New Client }
-- Executes when a new client appears.

client.connect_signal("manage", function(c)
	if awesome.startup and not c.size_hints.user_position and not c.size_hints.program_position then
		-- Prevent clients from being unreachable after screen count changes.
		awful.placement.no_offscreen(c)
	end

	c.border_width = beautiful.border_width
end)

-- { Sloppy Focus }
-- When Sloppy Focus is enabled, focus follows the mouse.

client.connect_signal("mouse::enter", function(c)
	c:emit_signal("request::activate", "mouse_enter", { raise = false })
end)

-- { Focus Change Events }

client.connect_signal("focus", function(c)
	c.border_color = beautiful.border_focus
end)

client.connect_signal("unfocus", function(c)
	c.border_color = beautiful.border_normal
end)

-- { Screen Geometry Change }
-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)

screen.connect_signal("property::geometry", wallpaper.set)
