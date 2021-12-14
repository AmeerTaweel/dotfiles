-- { Bottom Status Bar }

local wibox = require("wibox")
local hardware = require("hardware")
local widgets = require("ui.status_bar.widgets")

local M = {}

-- {{ Bottom Left Status Bar }}

M.left = {
    widgets.time,
    widgets.empty_spacer,
    widgets.date,
    widgets.empty_spacer,
    widgets.volume,
    layout = wibox.layout.fixed.horizontal
}

-- {{ Bottom Center Status Bar }}

M.center = { layout = wibox.layout.fixed.horizontal }

-- {{ Bottom Right Status Bar }}

M.right = {
    widgets.cpu,
    widgets.empty_spacer,
    widgets.ram,
    widgets.empty_spacer,
    widgets.disk,
    widgets.empty_spacer,
    widgets.net,
    layout = wibox.layout.fixed.horizontal
}

if hardware.device_type.is_laptop then
    M.right = {
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
    }
end

return M
