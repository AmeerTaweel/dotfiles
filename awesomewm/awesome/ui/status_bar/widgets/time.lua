local beautiful = require("beautiful")
local vicious_widget = require("ui.status_bar.widgets.vicious_widget")
local vicious = require("ui.status_bar.vicious")

local span = {
	open_tag = [[<span color="]] .. beautiful.widgets.time.fg .. [[" background="]] .. beautiful.widgets.time.bg .. [[">]],
	end_tag = "</span>"
}

return vicious_widget.create(vicious.widgets.date, span.open_tag .. " TIME: %H:%M:%S " .. span.end_tag, 1)
