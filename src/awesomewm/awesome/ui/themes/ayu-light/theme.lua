-- { Material Yellow Theme }

-- {{ Imports }}
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
local theme_manager = require("ui.theme-manager")

local colors = {
	grey = "#5C6166",
	black = "#000000",
	orange = "#F2AE49",
	dark_orange = "#FA8D3E",
	blue = "#399EE6",
	white = "#FCFCFC",
	green = "#86B300",
	purple = "#A37ACC",
	red = "#E65050",
}

local theme = {}

theme.name = "ayu-light"

-- {{ Font }}

-- Format: "%s %u", FONT_NAME, FONT_SIZE
theme.font = "Hack Nerd Font 10"


-- {{ Background Colors }}

theme.bg = {
	normal = colors.white,
	focus = colors.orange,
	urgent = colors.blue,
	minimize = colors.black
}
theme.bg.systray = theme.bg.normal

-- {{ Foreground Colors }}

theme.fg = {
	normal = colors.grey,
	focus = colors.black,
	urgent = colors.black,
	minimize = colors.black
}

-- {{ Gap }}

theme.gap = dpi(0)

-- {{ Border }}

theme.border = {
	width = dpi(3),
	normal = theme.bg.normal,
	focus = theme.bg.focus,
	marked = colors.red
}

-- {{ Widgets }}

theme.widgets = {}

theme.widgets.time = {
	bg = colors.purple,
	fg = colors.black
}

theme.widgets.date = {
	bg = colors.dark_orange,
	fg = colors.black
}

theme.widgets.volume = {
	bg = colors.blue,
	fg = colors.black
}

local levels_widget = {
	normal = { bg = colors.green, fg = colors.black },
	warning = { bg = colors.orange, fg = colors.black },
	danger = { bg = colors.red, fg = colors.black }
}

theme.widgets.cpu = levels_widget
theme.widgets.ram = levels_widget
theme.widgets.battery = levels_widget
theme.widgets.disk = levels_widget
theme.widgets.net = levels_widget

return theme_manager.create(theme)
