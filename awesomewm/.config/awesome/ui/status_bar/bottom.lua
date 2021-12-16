-- { Bottom Status Bar }

local awful = require("awful")
local wibox = require("wibox")
local hardware = require("hardware")
local widgets = require("ui.status_bar.widgets")

local M = {}

-- {{ Bottom Left Status Bar }}

M.left = function(s)
	return {
		widgets.time,
		widgets.empty_spacer,
		widgets.date,
		widgets.empty_spacer,
		widgets.volume,
		layout = wibox.layout.fixed.horizontal
	}
end

-- {{ Bottom Center Status Bar }}

M.center = function(s)
	return { layout = wibox.layout.fixed.horizontal }
end

-- {{ Bottom Right Status Bar }}

M.right_laptop = function(s)
	return {
		widgets.cpu,
		widgets.empty_spacer,
		widgets.ram,
		widgets.empty_spacer,
		widgets.battery,
		widgets.empty_spacer,
		widgets.disk,
		widgets.empty_spacer,
		widgets.net,
		layout = wibox.layout.fixed.horizontal
	}
end

M.right = function(s)
	if hardware.device_type.is_laptop then
		return M.right_laptop(s)
	end
	return {
		widgets.cpu,
		widgets.empty_spacer,
		widgets.ram,
		widgets.empty_spacer,
		widgets.disk,
		widgets.empty_spacer,
		widgets.net,
		layout = wibox.layout.fixed.horizontal
	}
end

M.setup = function(s)
	s.bottom_status_bar = awful.wibar({ position = "bottom", screen = s })
	s.bottom_status_bar:setup {
		M.left(s),
		M.center(s),
		M.right(s),
		layout = wibox.layout.align.horizontal
	}
end

return M
