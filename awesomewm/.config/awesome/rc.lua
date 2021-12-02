-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

-- TODO: command to edit config files
-- TODO: command to open manual pages
-- TODO: Fix touchpad double click middle
-- TODO: Rofi open tmuxinator project
-- TODO: Rofi open tmux session
-- TODO: Fix screenshot no
-- TODO: Rofi scripts fix
-- TODO: Startup programs fix
-- TODO: layouts fix
-- TODO: ui directory fix

require("error-handling")

-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")

-- Theme handling library
local beautiful = require("beautiful")
beautiful.init(require("ui.theme"))

-- Table of layouts to cover with awful.layout.inc (order matters)
local layouts = require("layouts")
awful.layout.layouts = layouts.list

-- Initialize screens
local tags = require("tags")
local wallpaper = require("ui.wallpaper")
local system_bar = require("ui.system-bar")
awful.screen.connect_for_each_screen(function(s)
	-- Each screen has its own tag table.
	awful.tag(tags.list, s, awful.layout.layouts[1])

	wallpaper.set(s)

	system_bar.setup(s)
end)

-- Set global key bindings
local global_bindings = require("bindings.global")
root.keys(global_bindings)

-- Set mouse bindings
local mouse_bindings = require("bindings.mouse")
root.buttons(mouse_bindings)

-- Rules to apply to new clients (through the "manage" signal).
local rules = require("rules")
awful.rules.rules = rules

-- Setup signal handlers
require("signals")

-- Launch startup commands and applications
require("startup")
