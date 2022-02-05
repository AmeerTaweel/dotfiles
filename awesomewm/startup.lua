local awful = require("awful")

-- { Commands }

-- Start clipboard manager
awful.spawn.with_shell("copyq")

-- { Applications }

-- Start communication applications
awful.spawn ("telegram-desktop")
awful.spawn ("slack")
awful.spawn ("discord")
