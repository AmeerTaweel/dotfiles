--[[
+------------------------------------+
| # VIM-TMUX Navigator Configuration |
+------------------------------------+
--]]

require "globals"

-- Disable VIM-TMUX navigator when zooming the VIM pane in TMUX
variables.global.tmux_navigator_disable_when_zoomed = 1
