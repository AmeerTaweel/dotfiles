-- { Windows List Status Bar }

local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")

local M = {}

M.buttons = gears.table.join(
	-- On leftclick, minimize if window is focused, and focus otherwise.
	awful.button({ }, 1, function (c)
		if c == client.focus then
			c.minimized = true
		else
			c:emit_signal(
				"request::activate",
				"tasklist",
				{ raise = true }
			)
		end
	end),
    -- View next window on scolling up
	awful.button({ }, 4, function ()
		awful.client.focus.byidx(1)
	end),
    -- View previous window on scolling down
	awful.button({ }, 5, function ()
		awful.client.focus.byidx(-1)
	end)
)

M.setup = function(s)
	s.windows_list = awful.widget.tasklist {
		screen  = s,
		filter  = awful.widget.tasklist.filter.currenttags,
		buttons = M.buttons
	}

	s.windows_list_status_bar = awful.wibar({ position = "bottom", screen = s })
	s.windows_list_status_bar:setup {
		{ layout = wibox.layout.fixed.horizontal },
		s.windows_list,
		{ layout = wibox.layout.fixed.horizontal },
		layout = wibox.layout.align.horizontal
	}
end

return M
