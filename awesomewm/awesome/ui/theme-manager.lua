-- { Material Yellow Theme }

local themes_path = "~/.config/awesome/ui/themes/"

local create = function(options)
	local theme = {}

	-- {{ Font }}

	-- Format: "%s %u", FONT_NAME, FONT_SIZE
	theme.font = options.font

	-- {{ Background Colors }}

	theme.bg_normal = options.bg.normal
	theme.bg_focus = options.bg.focus
	theme.bg_urgent = options.bg.urgent
	theme.bg_minimize = options.bg.minimize
	theme.bg_systray = options.bg.systray

	-- {{ Foreground Colors }}

	theme.fg_normal = options.fg.normal
	theme.fg_focus = options.fg.focus
	theme.fg_urgent = options.fg.urgent
	theme.fg_minimize = options.fg.minimize

	-- {{ Gap }}

	theme.useless_gap = options.gap

	-- {{ Border }}

	theme.border_width = options.border.width
	theme.border_normal = options.border.normal
	theme.border_focus = options.border.focus
	theme.border_marked = options.border.marked

	-- {{ Widgets }}

	theme.widgets = {}
	theme.widgets.time = options.widgets.time
	theme.widgets.date = options.widgets.date
	theme.widgets.volume = options.widgets.volume
	theme.widgets.cpu = options.widgets.cpu
	theme.widgets.ram = options.widgets.ram
	theme.widgets.battery = options.widgets.battery
	theme.widgets.disk = options.widgets.disk
	theme.widgets.net = options.widgets.net

	theme.wallpaper = themes_path .. options.name .. "/background.png"

	return theme
end

return {
	create = create
}
