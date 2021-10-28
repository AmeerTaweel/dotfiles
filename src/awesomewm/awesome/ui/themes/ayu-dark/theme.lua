-- { Material Yellow Theme }

-- {{ Imports }}
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
local theme_manager = require("ui.theme-manager")

local colors = {
	black = "#0D1017",
	orange = "#E6B450",
	dark_orange = "#FF8F40",
	blue = "#59C2FF",
	white = "#BFBDB6",
	green = "#AAD94C",
	purple = "#D2A6FF",
	red = "#D95757"
}

local theme = {}

theme.name = "ayu-dark"

-- {{ Font }}

-- Format: "%s %u", FONT_NAME, FONT_SIZE
theme.font = "Hack Nerd Font 10"


-- {{ Background Colors }}

theme.bg = {
	normal = colors.black,
	focus = colors.orange,
	urgent = colors.blue,
	minimize = colors.white
}
theme.bg.systray = theme.bg.normal

-- {{ Foreground Colors }}

theme.fg = {
	normal = colors.white,
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
	marked = colors.white
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
