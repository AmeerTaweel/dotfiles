local beautiful = require("beautiful")
local vicious_widget = require("ui.status_bar.widgets.vicious_widget")
local vicious = require("vicious")

local span = {
	open_tag = [[<span color="]] .. beautiful.widgets.date.fg .. [[" background="]] .. beautiful.widgets.date.bg .. [[">]],
	end_tag = "</span>"
}

return vicious_widget.create(vicious.widgets.date, span.open_tag .. " DATE: %a, %d %b " .. span.end_tag, 1)
