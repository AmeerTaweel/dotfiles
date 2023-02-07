-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

require("error-handling")

-- Standard awesome library
local awful = require("awful")
local gears = require("gears")
local beautiful = require("beautiful")
local wibox = require("wibox")
require("awful.autofocus")

-- Themes define colours, icons, font and wallpapers.
beautiful.init(gears.filesystem.get_themes_dir() .. "default/theme.lua")

-- Table of layouts to cover with awful.layout.inc (order matters)
local layouts = require("layouts")
awful.layout.layouts = layouts.list

-- Initialize screens
local tags = require("tags")
awful.screen.connect_for_each_screen(function(s)
    -- Each screen has its own tag table.
    awful.tag(tags.list, s, awful.layout.layouts[1])

    -- Create widgets
    local widgets = {}

    widgets.layout_box = awful.widget.layoutbox(s)

    widgets.tag_list = awful.widget.taglist {
        screen  = s,
        filter  = awful.widget.taglist.filter.all
    }

    widgets.kbd_layout = awful.widget.keyboardlayout()

    widgets.text_clock = wibox.widget.textclock()

    widgets.systray = wibox.widget.systray()

    -- Create the bar
    local bar = awful.wibar({ position = "top", screen = s })

    -- Add widgets to the bar
    bar:setup {
        layout = wibox.layout.align.horizontal,
        { -- Left widgets
            layout = wibox.layout.fixed.horizontal,
            widgets.tag_list
        }, {
            layout = wibox.layout.fixed.horizontal
        }, { -- Right widgets
            layout = wibox.layout.fixed.horizontal,
            widgets.kbd_layout,
            widgets.text_clock,
            widgets.layout_box
        }
    }
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
