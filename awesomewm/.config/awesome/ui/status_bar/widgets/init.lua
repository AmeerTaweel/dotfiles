-- { Statusbar Widgets }

local M = {}

M.time = require("ui.status_bar.widgets.time")
M.date = require("ui.status_bar.widgets.date")
M.volume = require("ui.status_bar.widgets.volume")
M.cpu = require("ui.status_bar.widgets.cpu")
M.ram = require("ui.status_bar.widgets.ram")
M.battery = require("ui.status_bar.widgets.battery")
M.empty_spacer = require("ui.status_bar.widgets.empty_spacer")
M.disk = require("ui.status_bar.widgets.disk")
M.net = require("ui.status_bar.widgets.net")

return M
