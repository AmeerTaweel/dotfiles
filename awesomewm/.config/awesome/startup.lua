-- { Startup Commands and Applications }

local awful = require("awful")

-- {{ Commands }}

-- CopyQ clipboard manager
awful.spawn.with_shell("copyq")
-- xmodmap keyboard key mappings
awful.spawn.with_shell("xmodmap .Xmodmap")
-- Set screen resolution with xrandr
awful.spawn.with_shell("xrandr -s 1440x900")
-- Turn on numlock
awful.spawn.with_shell("numlockx on")

-- {{ Applications }}

-- awful.spawn ("telegram-desktop")
-- awful.spawn ("slack")
-- awful.spawn ("discord")
