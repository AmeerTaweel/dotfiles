local beautiful = require("beautiful")
local vicious_widget = require("ui.status_bar.widgets.vicious_widget")
local vicious = require("vicious")

local format = function (_, args)
	local ram_usage = args[1]

	local usage_level = beautiful.widgets.ram.normal
	if ram_usage > 90 then
		usage_level = beautiful.widgets.ram.danger
	elseif ram_usage > 80 then
		usage_level = beautiful.widgets.ram.warning
	end

	local span = {
		open_tag = [[<span color="]] .. usage_level.fg .. [[" background="]] .. usage_level.bg .. [[">]],
		end_tag = "</span>"
	}

	return (span.open_tag .. " RAM: %02d%% " .. span.end_tag):format(ram_usage)
end

return vicious_widget.create(vicious.widgets.mem, format, 7)
