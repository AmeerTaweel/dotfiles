local awful = require("awful")
local beautiful = require("beautiful")

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
