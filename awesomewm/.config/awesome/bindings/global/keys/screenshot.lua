-- { Screenshot Key Bindings }

local awful = require("awful")

local keymap = {
	{
		"r",
		function()
			awful.spawn.with_shell("maim --select | xclip -selection clipboard -target image/png")
		end,
		"take screenshot of region"
	}, {
		"w",
		function()
			awful.spawn.with_shell("maim --window $(xdotool getactivewindow) | xclip -selection clipboard -target image/png")
		end,
		"take screenshot of current active window"
	}, {
		"f",
		function()
			awful.spawn.with_shell("maim | xclip -selection clipboard -target image/png")

		end,
		"take screenshot of screen"
	}
}

return keymap
