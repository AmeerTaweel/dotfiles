-- { Global Key Bindings }

local awful = require("awful")
local gears = require("gears")
local hotkeys_popup = require("awful.hotkeys_popup")
local vars = require("user-variables")
local modalbind = require("modalbind")

modalbind.init()
modalbind.set_location("centered")

modalbind.default_keys = {
	{"x", modalbind.close_box, "abort"}
}

local M = gears.table.join(
	-- General
	awful.key(
		{ vars.mod_key, "Control", "Shift" }, "h",
		hotkeys_popup.show_help,
		{ group = "general", description = "show help" }
	),
	awful.key(
		{ vars.mod_key, "Control" }, "r", awesome.restart,
		{ group = "general", description = "reload awesome" }
	),
	awful.key(
		{ vars.mod_key, "Shift" }, "q",
		awesome.quit,
		{ group = "general", description = "quite awesome" }
	),

	-- Tags
	awful.key(
		{ vars.mod_key, "Control" }, "k",
		awful.tag.viewprev,
		{ group = "tag", description = "go to previous tag" }
	),
	awful.key(
		{ vars.mod_key, "Control" }, "j",
		awful.tag.viewnext,
		{ group = "tag", description = "go to next tag" }
	),
	awful.key(
		{ vars.mod_key }, "Escape",
		awful.tag.history.restore,
		{ group = "tag", description = "go to last tag" }
	),

	-- Client
	awful.key(
		{ vars.mod_key }, "j",
		function () awful.client.focus.byidx(1) end,
		{ group = "client", description = "go to next client by index" }
	),
	awful.key(
		{ vars.mod_key }, "k",
		function () awful.client.focus.byidx(-1) end,
		{ group = "client", description = "go to previous client by index" }
	),
	awful.key(
		{ vars.mod_key, "Shift" }, "j",
		function () awful.client.swap.byidx(1) end,
		{ group = "client", description = "swap with next client by index" }
	),
	awful.key(
		{ vars.mod_key, "Shift" }, "k",
		function () awful.client.swap.byidx(-1) end,
		{ group = "client", description = "swap with previous client by index" }
	),
	awful.key(
		{ vars.mod_key }, "u",
		awful.client.urgent.jumpto,
		{ group = "client", description = "go to urgent client" }
	),
	awful.key(
		{ vars.mod_key }, "Tab",
		function ()
			awful.client.focus.history.previous()
			if client.focus then
				client.focus:raise()
			end
		end,
		{ group = "client", description = "go to last client" }
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
		{ group = "client", description = "unminimize the last minimized client" }
	),

	-- Screen
	awful.key(
		{ vars.mod_key, "Control", "Shift" }, "j",
		function () awful.screen.focus_relative(1) end,
		{ group = "screen", description = "go to the next screen" }
	),
	awful.key(
		{ vars.mod_key, "Control", "Shift" }, "k",
		function () awful.screen.focus_relative(-1) end,
		{ group = "screen", description = "go to the previous screen" }
	),

	-- Layout
	awful.key(
		{ vars.mod_key }, "l",
		function () awful.tag.incmwfact(0.05) end,
		{ group = "layout", description = "increase master width" }
	),
	awful.key(
		{ vars.mod_key }, "h",
		function () awful.tag.incmwfact(-0.05) end,
		{ group = "layout", description = "decrease master width" }
	),
	awful.key(
		{ vars.mod_key, "Shift" }, "h",
		function () awful.tag.incnmaster(1, nil, true) end,
		{ group = "layout", description = "increase master clients number" }
	),
	awful.key(
		{ vars.mod_key, "Shift" }, "l",
		function () awful.tag.incnmaster(-1, nil, true) end,
		{ group = "layout", description = "decrease master clients number" }
	),
	awful.key(
		{ vars.mod_key, "Control" }, "h",
		function () awful.tag.incncol(1, nil, true) end,
		{ group = "layout", description = "increase number of columns" }
	),
	awful.key(
		{ vars.mod_key, "Control" }, "l",
		function () awful.tag.incncol(-1, nil, true) end,
		{ group = "layout", description = "decrease number of columns" }
	),
	awful.key(
		{ vars.mod_key }, "space",
		function () awful.layout.inc(1) end,
		{ group = "layout", description = "select next layout" }
	),
	awful.key(
		{ vars.mod_key, "Shift" }, "space",
		function () awful.layout.inc(-1) end,
		{ group = "layout", description = "select previous layout" }
	),

	-- Launch
	awful.key(
		{ vars.mod_key }, "Return",
		function () awful.spawn(vars.terminal) end,
		{ group = "launch", description = "open a terminal" }
	),
	awful.key(
		{ vars.mod_key }, "b",
		function () awful.spawn(vars.browser) end,
		{ group = "launch", description = "open a browser" }
	),
	awful.key(
		{ vars.mod_key }, "p",
		function() awful.util.spawn("rofi -show drun -display-drun 'launch'") end,
		{ group = "launch", description = "launch a program" }
	),
	awful.key(
		{ vars.mod_key }, "r",
		function() awful.util.spawn("rofi -show run") end,
		{ group = "launch", description = "run a command" }
	),
	awful.key(
		{ vars.mod_key }, "w",
		function() awful.util.spawn("rofi -show windowcd -display-windowcd 'window'") end,
		{ group = "launch", description = "switch window within current tag" }
	),
	awful.key(
		{ vars.mod_key, "Shift" }, "w",
		function() awful.util.spawn("rofi -show window") end,
		{ group = "launch", description = "switch window" }
	),
	awful.key(
		{ vars.mod_key }, "s",
		function() awful.util.spawn("rofi -show ssh -no-parse-known-hosts -disable-history") end,
		{ group = "launch", description = "start ssh connection" }
	),
	awful.key(
		{ vars.mod_key, "Control" }, "p",
		function() 
			local power_options = {
				["lock\n"] = "light-locker-command -l",
				["suspend\n"] = "systemctl suspend",
				["hibernate\n"] = "systemctl hibernate",
				["power off\n"] = "systemctl poweroff",
				["reboot\n"] = "systemctl reboot",
				["logout\n"] = "awesome restart --replace"
			}
			local command = [[bash -c 'echo -e "lock\nsuspend\nhibernate\npower off\nreboot\nlogout" | rofi -dmenu -p "power options" -l 6 -width 20']]
			awful.spawn.easy_async(command, function(stdout)
				awful.spawn(power_options[stdout])
			end)
		end,
		{ group = "launch", description = "power options" }
	),

	-- Volume Controls
	awful.key({}, "XF86AudioRaiseVolume", function ()
		local vol = require("ui.status_bar.widgets.volume")
		local v = require("vicious")
		awful.spawn.easy_async("pamixer --increase 5 --allow-boost", function() v.force({ vol }) end)
	end),
	awful.key({}, "XF86AudioLowerVolume", function ()
		local vol = require("ui.status_bar.widgets.volume")
		local v = require("vicious")
		awful.spawn.easy_async("pamixer --decrease 5 --allow-boost", function() v.force({ vol }) end)
	end),
	awful.key({}, "XF86AudioMute", function ()
		local vol = require("ui.status_bar.widgets.volume")
		local v = require("vicious")
		awful.spawn.easy_async("pamixer --toggle-mute --allow-boost", function() v.force({ vol }) end)
	end),
	awful.key({ vars.mod_key }, "x", function() modalbind.grab{
		name = "screenshot",
		keymap = require("bindings.global.screenshot"),
		stay_in_mode = false
	} end,
	{ group = "launch", description = "take a screenshot" }
	)
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
	M = gears.table.join(M,
		-- View tag only
		awful.key(
			{ vars.mod_key }, "#" .. i + 9,
			function()
				local screen = awful.screen.focused()
				local tag = screen.tags[i]
				if tag then tag:view_only() end
			end,
			{ group = "tag", description = "view tag #" .. i }
		),
		-- Toggle tag display
		awful.key(
			{ vars.mod_key, "Control" }, "#" .. i + 9,
			function()
				local screen = awful.screen.focused()
				local tag = screen.tags[i]
				if tag then awful.tag.viewtoggle(tag) end
			end,
			{ group = "tag", description = "toggle tag #" .. i }
		),
		-- Move client to tag.
		awful.key(
			{ vars.mod_key, "Shift" }, "#" .. i + 9,
			function()
				if client.focus then
					local tag = client.focus.screen.tags[i]
					if tag then	client.focus:move_to_tag(tag) end
				end
			end,
			{ group = "tag", description = "move focused client to tag #"..i }
		),
		-- Toggle tag on focused client.
		awful.key(
			{ vars.mod_key, "Control", "Shift" }, "#" .. i + 9,
			function()
				if client.focus then
					local tag = client.focus.screen.tags[i]
					if tag then client.focus:toggle_tag(tag) end
				end
			end,
			{ group = "tag", description = "toggle focused client on tag #" .. i }
		)
	)
end

return M
