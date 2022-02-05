local beautiful = require("beautiful")
local vicious_widget = require("ui.status_bar.widgets.vicious_widget")
local vicious = require("vicious")

local format = function (_, args)
	local volume = args[1]
	local is_muted = args[2] == "🔈"

	local style = beautiful.widgets.volume
	local span = {
		open_tag = [[<span color="]] .. style.fg .. [[" background="]] .. style.bg .. [[">]],
		end_tag = "</span>"
	}

	local mute_status = "U"
	if is_muted then
		mute_status = "M"
	end

	return (span.open_tag .. " VOLUME: %3s %1s " .. span.end_tag):format(volume, mute_status)
end

return vicious_widget.create(vicious.widgets.volume, format, 1, "Master")
