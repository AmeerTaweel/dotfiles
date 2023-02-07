-- { Global Keyboard Bindings }

local awful = require("awful")
local gears = require("gears")
local hotkeys_popup = require("awful.hotkeys_popup")
local vars = require("user-variables")

local M = gears.table.join(
	-- General
	awful.key(
		{ vars.mod_key, "Control" }, "h",
		hotkeys_popup.show_help,
		{ group = "general", description = "show help" }
	),
	awful.key(
		{ vars.mod_key, "Control" }, "r", awesome.restart,
		{ group = "general", description = "reload awesome" }
	),
	awful.key(
		{ vars.mod_key, "Control" }, "q",
		awesome.quit,
		{ group = "general", description = "quite awesome" }
	),

	-- Tags
	awful.key(
		{ vars.mod_key }, "p",
		awful.tag.viewprev,
		{ group = "tag", description = "go to previous tag" }
	),
	awful.key(
		{ vars.mod_key }, "n",
		awful.tag.viewnext,
		{ group = "tag", description = "go to next tag" }
	),
	awful.key(
		{ vars.mod_key }, "=",
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
		{ vars.mod_key }, "Tab",
		function ()
			awful.client.focus.history.previous()
			if client.focus then
				client.focus:raise()
			end
		end,
		{ group = "client", description = "go to last client" }
	),

	-- Layout
	awful.key(
		{ vars.mod_key }, "space",
		function () awful.layout.inc(1) end,
		{ group = "layout", description = "select next layout" }
	),
	awful.key(
		{ vars.mod_key, "Shift" }, "space",
		function () awful.layout.inc(-1) end,
		{ group = "layout", description = "select previous layout" }
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
		)
	)
end

return M
