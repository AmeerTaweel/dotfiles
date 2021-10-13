-- { Material Yellow Theme }

-- {{ Imports }}
local theme_assets = require("beautiful.theme_assets")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi

local gfs = require("gears.filesystem")
local themes_path = gfs.get_themes_dir()

local M = {}

-- {{ Font }}

-- Format: "%s %u", FONT_NAME, FONT_SIZE
M.font = "Hack Nerd Font 9"

-- {{ Background Colors }}

M.bg_normal     = "#000000"
M.bg_focus      = "#FFEB3B"
M.bg_urgent     = "#F44336"
M.bg_minimize   = M.bg_normal
M.bg_systray    = M.bg_normal

-- {{ Foreground Colors }}

M.fg_normal     = "#FFFFFF"
M.fg_focus      = "#000000"
M.fg_urgent     = "#FFFFFF"
M.fg_minimize   = M.fg_normal

-- {{ Gap }}
M.useless_gap = dpi(5)

-- {{ Border }}

M.border_width = dpi(5)
M.border_normal = "#000000"
M.border_focus = "#FFEB3B"
M.border_marked = "#F44336"

-- There are other variable sets
-- overriding the default one when
-- defined, the sets are:
-- taglist_[bg|fg]_[focus|urgent|occupied|empty|volatile]
-- tasklist_[bg|fg]_[focus|urgent]
-- titlebar_[bg|fg]_[normal|focus]
-- tooltip_[font|opacity|fg_color|bg_color|border_width|border_color]
-- mouse_finder_[color|timeout|animate_timeout|radius|factor]
-- prompt_[fg|bg|fg_cursor|bg_cursor|font]
-- hotkeys_[bg|fg|border_width|border_color|shape|opacity|modifiers_fg|label_bg|label_fg|group_margin|font|description_font]

-- Variables set for theming notifications:
-- notification_font
-- notification_[bg|fg]
-- notification_[width|height|margin]
-- notification_[border_color|border_width|shape|opacity]

-- Variables set for theming the menu:
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
M.menu_submenu_icon = themes_path.."default/submenu.png"
M.menu_height = dpi(15)
M.menu_width  = dpi(100)

-- You can add as many variables as
-- you wish and access them by using
-- beautiful.variable in your rc.lua
--theme.bg_widget = "#cc0000"

M.wallpaper = themes_path.."default/background.png"
M.wallpaper = "/home/ameer-taweel/Documents/dotfiles/src/awesomewm/awesome/ui/wallpapers/yellow-forest.jpg"

return M
