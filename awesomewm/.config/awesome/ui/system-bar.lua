-- { System Bar - Wibox }

local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")
local vars = require("user-variables")

local widgets = {}
widgets.line_separator = require("ui.status_bar.widgets.line_separator")
widgets.time = require("ui.status_bar.widgets.time")
widgets.date = require("ui.status_bar.widgets.date")
widgets.volume = require("ui.status_bar.widgets.volume")
widgets.cpu = require("ui.status_bar.widgets.cpu")
widgets.ram = require("ui.status_bar.widgets.ram")
widgets.battery = require("ui.status_bar.widgets.battery")
widgets.empty_spacer = require("ui.status_bar.widgets.empty_spacer")
widgets.disk = require("ui.status_bar.widgets.disk")
widgets.net = require("ui.status_bar.widgets.net")

-- Keyboard map indicator and switcher
mykeyboardlayout = awful.widget.keyboardlayout()

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
    -- Indicate which layout is in use
    s.layout_box = awful.widget.layoutbox(s)

    -- Create a taglist widget
    s.mytaglist = awful.widget.taglist {
        screen  = s,
        filter  = awful.widget.taglist.filter.all,
        buttons = taglist_buttons
    }

    -- Create the top status bar
    s.top_status_bar = awful.wibar({ position = "top", screen = s })
    s.top_status_bar:setup {
        {
            s.mytaglist,
            s.mypromptbox,
            layout = wibox.layout.fixed.horizontal
        },
		{ layout = wibox.layout.fixed.horizontal },
        {
            -- mykeyboardlayout,
            wibox.widget.systray(),
            s.layout_box,
            layout = wibox.layout.fixed.horizontal
        },
        layout = wibox.layout.align.horizontal
    }

    -- Create the bottom status bar
    s.bottom_status_bar = awful.wibar({ position = "bottom", screen = s })
    s.bottom_status_bar:setup {
        {
			widgets.time,
			widgets.empty_spacer,
			widgets.date,
			widgets.empty_spacer,
			widgets.volume,
            layout = wibox.layout.fixed.horizontal
        },
		{ layout = wibox.layout.fixed.horizontal },
        {
			widgets.cpu,
			widgets.empty_spacer,
			widgets.ram,
			widgets.empty_spacer,
			widgets.battery,
			widgets.empty_spacer,
			widgets.disk,
			widgets.empty_spacer,
			widgets.net,
            layout = wibox.layout.fixed.horizontal
        },
        layout = wibox.layout.align.horizontal
    }

    -- Create a tasklist widget
    s.task_list = awful.widget.tasklist {
        screen  = s,
        filter  = awful.widget.tasklist.filter.currenttags,
        buttons = tasklist_buttons
    }

    -- Create the task list bar
    s.task_list_bar = awful.wibar({ position = "bottom", screen = s })
    s.task_list_bar:setup {
		{ layout = wibox.layout.fixed.horizontal },
		s.task_list,
		{ layout = wibox.layout.fixed.horizontal },
        layout = wibox.layout.align.horizontal
    }
end

local M  = {
	setup = setup_system_bar
}

return M
