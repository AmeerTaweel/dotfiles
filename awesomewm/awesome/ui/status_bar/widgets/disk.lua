local beautiful = require("beautiful")
local vicious_widget = require("ui.status_bar.widgets.vicious_widget")
local vicious = require("ui.status_bar.vicious")

local format = function (_, args)
	local disk_usage = args["{/ used_p}"]

	local usage_level = beautiful.widgets.disk.normal
	if disk_usage > 90 then
		usage_level = beautiful.widgets.disk.danger
	elseif disk_usage > 80 then
		usage_level = beautiful.widgets.disk.warning
	end

	local span = {
		open_tag = [[<span color="]] .. usage_level.fg .. [[" background="]] .. usage_level.bg .. [[">]],
		end_tag = "</span>"
	}

	return (span.open_tag .. " DISK: %03s%% " .. span.end_tag):format(disk_usage)
end

local widget = vicious_widget.create(vicious.widgets.fs, format, 3600)

-- Initial draw while waiting for data
local default_level = beautiful.widgets.disk.normal
local span = {
	open_tag = [[<span color="]] .. default_level.fg .. [[" background="]] .. default_level.bg .. [[">]],
	end_tag = "</span>"
}
widget.markup = (span.open_tag .. " DISK: ???%% " .. span.end_tag):format()

return widget
