local awful = require("awful")
local gears = require("gears")
local vars = require("user-variables")

local M = {}

M.buttons = gears.table.join(
    -- View tag on leftclick
	awful.button({ }, 1, function(t) t:view_only() end),
    -- Move active window to tag on Mod + leftclick
	awful.button({ vars.mod_key }, 1, function(t)
		if client.focus then
			client.focus:move_to_tag(t)
        end
    end),
    -- View multiple tags on rightclick
	awful.button({ }, 3, awful.tag.viewtoggle),
    -- Toggle multiple tags on active window on Mod + rightclick
	awful.button({ vars.mod_key }, 3, function(t)
		if client.focus then
			client.focus:toggle_tag(t)
        end
    end),
    -- View next tag on scolling up
	awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
    -- View previous tag on scolling down
    awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
)

M.setup = function(s)
    s.tag_list = awful.widget.taglist {
        screen  = s,
        filter  = awful.widget.taglist.filter.all,
        buttons = M.buttons
    }
end

return M
