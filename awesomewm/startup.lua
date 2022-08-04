local awful = require("awful")

-- { Commands }

-- Start clipboard manager
awful.spawn.with_shell("copyq")

-- Start MPD and mpDris2
awful.spawn.with_shell("mpd")
awful.spawn.with_shell("mpDris2")

-- { Applications }

-- Start communication applications
awful.spawn ("slack")
awful.spawn ("discord")
awful.spawn ("telegram-desktop")
awful.spawn ("element-desktop")
