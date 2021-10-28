-- { Global Key Bindings }

local awful = require("awful")
local gears = require("gears")
local hotkeys_popup = require("awful.hotkeys_popup")
local vars = require("main.user-variables")

local M = gears.table.join(
    awful.key(
		{ vars.mod_key }, "h",
		hotkeys_popup.show_help,
		{ group="awesome", description="show help" }
	),
    awful.key(
		{ vars.mod_key }, "Left",
		awful.tag.viewprev,
        { description = "view previous", group = "tag" }
	),
    awful.key(
		{ vars.mod_key }, "Right",
		awful.tag.viewnext,
        { description = "view next", group = "tag" }
	),
    awful.key(
		{ vars.mod_key }, "Escape",
		awful.tag.history.restore,
        { description = "go back", group = "tag" }
	),
    awful.key(
		{ vars.mod_key }, "j",
        function ()
            awful.client.focus.byidx(1)
        end,
        { description = "focus next by index", group = "client" }
    ),
    awful.key(
		{ vars.mod_key }, "k",
        function ()
            awful.client.focus.byidx(-1)
        end,
        { description = "focus previous by index", group = "client" }
    ),
    -- Layout manipulation
    awful.key(
		{ vars.mod_key, "Shift" }, "j",
		function () awful.client.swap.byidx(1) end,
        { description = "swap with next client by index", group = "client" }
	),
    awful.key(
		{ vars.mod_key, "Shift" }, "k",
		function () awful.client.swap.byidx(-1) end,
        { description = "swap with previous client by index", group = "client" }
	),
    awful.key(
		{ vars.mod_key, "Control" }, "j",
		function () awful.screen.focus_relative(1) end,
        { description = "focus the next screen", group = "screen" }
	),
    awful.key(
		{ vars.mod_key, "Control" }, "k",
		function () awful.screen.focus_relative(-1) end,
        { description = "focus the previous screen", group = "screen" }
	),
    awful.key(
		{ vars.mod_key }, "u",
		awful.client.urgent.jumpto,
        { description = "jump to urgent client", group = "client" }
	),
    awful.key(
		{ vars.mod_key }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
				client.focus:raise()
            end
        end,
        { description = "go back", group = "client" }
	),

    -- Standard program
    awful.key(
		{ vars.mod_key }, "Return",
		function () awful.spawn(vars.terminal) end,
        { description = "open a terminal", group = "launcher" }
	),
    awful.key(
		{ vars.mod_key, "Control" }, "r", awesome.restart,
        { description = "reload awesome", group = "awesome" }
	),
    awful.key(
		{ vars.mod_key, "Shift" }, "q",
		awesome.quit,
        { description = "quit awesome", group = "awesome" }
	),
    awful.key(
		{ vars.mod_key }, "l",
		function () awful.tag.incmwfact(0.05) end,
        { description = "increase master width factor", group = "layout" }
	),
    awful.key(
		{ vars.mod_key }, "h",
		function () awful.tag.incmwfact(-0.05) end,
        { description = "decrease master width factor", group = "layout" }
	),
    awful.key(
		{ vars.mod_key, "Shift" }, "h",
		function () awful.tag.incnmaster(1, nil, true) end,
        { description = "increase the number of master clients", group = "layout" }
	),
    awful.key(
		{ vars.mod_key, "Shift" }, "l",
		function () awful.tag.incnmaster(-1, nil, true) end,
        { description = "decrease the number of master clients", group = "layout" }
	),
    awful.key(
		{ vars.mod_key, "Control" }, "h",
		function () awful.tag.incncol(1, nil, true) end,
        { description = "increase the number of columns", group = "layout" }
	),
    awful.key(
		{ vars.mod_key, "Control" }, "l",
		function () awful.tag.incncol(-1, nil, true) end,
        { description = "decrease the number of columns", group = "layout" }
	),
    awful.key(
		{ vars.mod_key }, "space",
		function () awful.layout.inc(1) end,
        { description = "select next", group = "layout" }
	),
    awful.key(
		{ vars.mod_key, "Shift" }, "space",
		function () awful.layout.inc(-1) end,
        { description = "select previous", group = "layout" }
	),
    awful.key(
		{ vars.mod_key, "Control" }, "n",
        function ()
			local c = awful.client.restore()
			-- Focus restored client
			if c then
				c:emit_signal("request::activate", "key.unminimize", {raise = true})
            end
        end,
        { description = "restore minimized", group = "client" }
	),

    -- Menubar
    awful.key({ vars.mod_key }, "p", function() awful.util.spawn('rofi -show drun') end,
              {description = "show the menubar", group = "launcher"}),
    awful.key({ vars.mod_key }, "r", function() awful.util.spawn('rofi -show run') end,
              {description = "show the menubar", group = "launcher"}),
	awful.key({}, "XF86AudioRaiseVolume", function ()
		local vol = require("ui.status_bar.widgets.volume")
		local v = require("ui.status_bar.vicious")
		awful.spawn.easy_async("amixer -D pulse sset Master 5%+", function()
			v.force({ vol })
		end)
	end),
	awful.key({}, "XF86AudioLowerVolume", function ()
		local vol = require("ui.status_bar.widgets.volume")
		local v = require("ui.status_bar.vicious")
		awful.spawn.easy_async("amixer -D pulse sset Master 5%-", function()
			v.force({ vol })
		end)
	end),
	awful.key({}, "XF86AudioMute", function ()
		local vol = require("ui.status_bar.widgets.volume")
		local v = require("ui.status_bar.vicious")
		awful.spawn.easy_async("amixer -D pulse sset Master toggle", function()
			v.force({ vol })
		end)
	end)
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
	M = gears.table.join(M,
        -- View tag only.
        awful.key({ vars.mod_key }, "#" .. i + 9,
                  function ()
                        local screen = awful.screen.focused()
                        local tag = screen.tags[i]
                        if tag then
                           tag:view_only()
                        end
                  end,
                  {description = "view tag #"..i, group = "tag"}),
        -- Toggle tag display.
        awful.key({ vars.mod_key, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = awful.screen.focused()
                      local tag = screen.tags[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end,
                  {description = "toggle tag #" .. i, group = "tag"}),
        -- Move client to tag.
        awful.key({ vars.mod_key, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:move_to_tag(tag)
                          end
                     end
                  end,
                  {description = "move focused client to tag #"..i, group = "tag"}),
        -- Toggle tag on focused client.
        awful.key({ vars.mod_key, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:toggle_tag(tag)
                          end
                      end
                  end,
                  {description = "toggle focused client on tag #" .. i, group = "tag"})
    )
end

clientbuttons = gears.table.join(
    awful.button({ }, 1, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
    end),
    awful.button({ vars.mod_key }, 1, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
        awful.mouse.client.move(c)
    end),
    awful.button({ vars.mod_key }, 3, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
        awful.mouse.client.resize(c)
    end)
)


return M
