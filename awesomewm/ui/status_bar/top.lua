local awful = require("awful")
local wibox = require("wibox")
local widgets = require("ui.status_bar.widgets")

local M = {}

-- { Top Left Status Bar }

M.left = function(s)
	widgets.tag_list.setup(s)
	return {
		s.tag_list,
		layout = wibox.layout.fixed.horizontal
	}
end

-- { Top Center Status Bar }

M.center = function(s)
	return { layout = wibox.layout.fixed.horizontal }
end

-- { Top Right Status Bar }

M.right = function(s)
	return {
		awful.widget.keyboardlayout(), -- keyboard layout
		wibox.widget.systray(), -- system tray
		awful.widget.layoutbox(s), -- indicate which layout is in use
		layout = wibox.layout.fixed.horizontal
	}
end

M.setup = function(s)
	s.top_status_bar = awful.wibar({ position = "top", screen = s })
	s.top_status_bar:setup {
		M.left(s),
		M.center(s),
		M.right(s),
		layout = wibox.layout.align.horizontal
	}
end

return M
