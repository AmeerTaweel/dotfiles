local beautiful = require("beautiful")
local vicious_widget = require("ui.status_bar.widgets.vicious_widget")
local vicious = require("ui.status_bar.vicious")

local format = function (_, args)
	local is_charging = args[1] == "+"
	local percentage = args[2]

	local charging_text = "++"
	if not is_charging then
		charging_text = "--"
	end

	local battery_level = beautiful.widgets.battery.normal
	if percentage <= 20 and not is_charging then
		battery_level = beautiful.widgets.battery.danger
	elseif percentage <= 50 and not is_charging then
		battery_level = beautiful.widgets.battery.warning
	end

	local span = {
		open_tag = [[<span color="]] .. battery_level.fg .. [[" background="]] .. battery_level.bg .. [[">]],
		end_tag = "</span>"
	}

	return (span.open_tag .. " BATTERY: %3s%% %s " .. span.end_tag):format(percentage, charging_text)
end

return vicious_widget.create(vicious.widgets.bat, format, 1, "BAT1")
