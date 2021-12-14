-- { Startup Commands and Applications }

local awful = require("awful")
local hardware = require("hardware")

-- {{ Commands }}

-- CopyQ clipboard manager
awful.spawn.with_shell("copyq")
-- xmodmap keyboard key mappings
awful.spawn.with_shell("xmodmap .Xmodmap")
-- Set screen resolution with xrandr
awful.spawn.with_shell("xrandr -s 1440x900")
-- Turn on numlock
awful.spawn.with_shell("numlockx on")

if hardware.device_type.is_laptop then
	-- Enable tapping on laptop touchpads
	awful.spawn.with_shell("xinput set-prop '$(xinput list --name-only | grep -i touch)' 'libinput Tapping Enabled' 1")
end

-- {{ Applications }}

-- awful.spawn ("telegram-desktop")
-- awful.spawn ("slack")
-- awful.spawn ("discord")
