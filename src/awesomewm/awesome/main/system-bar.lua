-- { System Bar - Wibox }

local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")
local vars = require("main.user-variables")

-- Keyboard map indicator and switcher
mykeyboardlayout = awful.widget.keyboardlayout()

-- Create a textclock widget
mytextclock = wibox.widget.textclock()

-- Create a wibox for each screen and add it
local taglist_buttons = gears.table.join(
	awful.button({ }, 1, function(t) t:view_only() end),
	awful.button({ vars.mod_key }, 1, function(t)
		if client.focus then
			client.focus:move_to_tag(t)
        end
    end),
	awful.button({ }, 3, awful.tag.viewtoggle),
	awful.button({ vars.mod_key }, 3, function(t)
		if client.focus then
			client.focus:toggle_tag(t)
        end
    end),
	awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
    awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
)

local tasklist_buttons = gears.table.join(
	awful.button({ }, 1, function (c)
		if c == client.focus then
			c.minimized = true
        else
			c:emit_signal(
				"request::activate",
				"tasklist",
				{ raise = true }
			)
        end
    end),
	awful.button({ }, 3, function()
		awful.menu.client_list({ theme = { width = 250 } })
    end),
    awful.button({ }, 4, function ()
		awful.client.focus.byidx(1)
    end),
	awful.button({ }, 5, function ()
		awful.client.focus.byidx(-1)
    end)
)

local setup_system_bar = function(s)
    -- Create an imagebox widget which will contain an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    s.mylayoutbox = awful.widget.layoutbox(s)
    s.mylayoutbox:buttons(gears.table.join(
		awful.button({ }, 1, function () awful.layout.inc( 1) end),
		awful.button({ }, 3, function () awful.layout.inc(-1) end),
		awful.button({ }, 4, function () awful.layout.inc( 1) end),
		awful.button({ }, 5, function () awful.layout.inc(-1) end)
	))
    -- Create a taglist widget
    s.mytaglist = awful.widget.taglist {
        screen  = s,
        filter  = awful.widget.taglist.filter.all,
        buttons = taglist_buttons
    }

    -- Create a tasklist widget
    -- s.mytasklist = awful.widget.tasklist {
    --     screen  = s,
    --     filter  = awful.widget.tasklist.filter.currenttags,
    --     buttons = tasklist_buttons
    -- }

    -- Create the wibox
    s.mywibox = awful.wibar({ position = "top", screen = s })

    -- Add widgets to the wibox
    s.mywibox:setup {
        layout = wibox.layout.align.horizontal,
        { -- Left widgets
            layout = wibox.layout.fixed.horizontal,
            -- mylauncher,
            s.mytaglist,
            s.mypromptbox,
        },
        s.mytasklist, -- Middle widget
        { -- Right widgets
            layout = wibox.layout.fixed.horizontal,
            mykeyboardlayout,
            wibox.widget.systray(),
            mytextclock,
            s.mylayoutbox,
        },
    }
end

local M  = {
	setup = setup_system_bar
}

return M
