-- { Startup Commands and Applications }

local awful = require("awful")
local hardware = require("hardware")

-- {{ Commands }}

-- copyq - clipboard manager
awful.spawn.with_shell("copyq")

-- xmodmap - keyboard key mappings
awful.spawn.with_shell("xmodmap .Xmodmap")

-- numlockx - enable numlock by default
awful.spawn.with_shell("numlockx on")

-- bluetooth applet
-- awful.spawn.with_shell("blueman-applet")

-- {{ Applications }}

awful.spawn ("telegram-desktop")
awful.spawn ("slack")
awful.spawn ("discord")
