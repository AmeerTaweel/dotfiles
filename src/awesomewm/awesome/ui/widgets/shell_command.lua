local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")

local default_options = {
	bg = beautiful.bg_normal, -- Theme background
	fg = beautiful.fg_normal, -- Theme foreground
	pre_text = "", -- Text to display before the output of the command
	refresh_rate = 1 -- Refresh every second
}

local get_refresh_callback = function(pre_text)
	return function(widget, output, error, error_msg, error_code)
		widget:set_text(pre_text .. output)
	end
end

local merge = function(t1, t2)
	local merged = {}
	for k, v in pairs(t1) do
		merged[k] = v
	end
	for k, v in pairs(t2) do
		merged[k] = v
	end
	return merged
end


local setup = function(command, custom_options)
	local options = merge(default_options, custom_options)
	local refresh_callback = get_refresh_callback(options.pre_text)

	-- Return widget
	return {
		{
			align = "center",
			widget = awful.widget.watch(
				command,
				options.refresh_rate,
				refresh_callback
			),
		},
		bg = options.bg,
		fg = options.fg,
		widget = wibox.container.background
	}
end

return {
	setup = setup
}
