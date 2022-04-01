local beautiful = require("beautiful")
local vicious_widget = require("ui.status_bar.widgets.vicious_widget")
local vicious = require("vicious")

local format = function (_, args)
	local cpu_usage = args[1]

	local usage_level = beautiful.widgets.cpu.normal
	if cpu_usage > 80 then
		usage_level = beautiful.widgets.cpu.danger
	elseif cpu_usage > 50 then
		usage_level = beautiful.widgets.cpu.warning
	end

	local span = {
		open_tag = [[<span color="]] .. usage_level.fg .. [[" background="]] .. usage_level.bg .. [[">]],
		end_tag = "</span>"
	}

	return (span.open_tag .. " CPU: %02d%% " .. span.end_tag):format(cpu_usage)
end

return vicious_widget.create(vicious.widgets.cpu, format, 5)
