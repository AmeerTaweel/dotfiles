-- { Internet Connectivity Widget }
local awful = require("awful")
local beautiful = require("beautiful")

local command = "ping -c1 www.google.com -W 2"

local format = function(widget, _, std_err, _, _)
	local is_connected = std_err == nil or std_err == ""
	local connectivity_text = "CONNECTED"
	local status = beautiful.widgets.net.connected
	if not is_connected then
		connectivity_text = "DISCONNECTED"
		status = beautiful.widgets.net.disconnected
	end

	local span = {
		open_tag = [[<span color="]] .. status.fg .. [[" background="]] .. status.bg .. [[">]],
		end_tag = "</span>"
	}

	widget.markup = (span.open_tag .. " NET: %-12s " .. span.end_tag):format(connectivity_text)
end

return awful.widget.watch(command, 5, format)
