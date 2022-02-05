-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

--[[
TODO: Fix the following files:
	+ bindings/*/keys.lua -> global and client key bindings
	+ layouts.lua
	+ tags.lua
--]]

--[[
TODO: Create and integrate with custom Rofi scripts:
	+ TMUX session launcher
--]]

--[[
TODO: Write a Vicious widget that use pamixer rather than amixer

This helps us get rid of the alsa-utils dependency
--]]

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
local status_bar = require("ui.status_bar")
awful.screen.connect_for_each_screen(function(s)
	-- Each screen has its own tag table.
	awful.tag(tags.list, s, awful.layout.layouts[1])

	wallpaper.set(s)

	status_bar.setup(s)
end)

-- Set global key bindings
root.keys(require("bindings.global.keys"))

-- Set global mouse bindings
root.buttons(require("bindings.global.buttons"))

-- Rules to apply to new clients (through the "manage" signal).
local rules = require("rules")
awful.rules.rules = rules

-- Setup signal handlers
require("signals")

-- Launch startup commands and applications
require("startup")
