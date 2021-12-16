-- { Status Bar }

local M = {}

-- Setup status bars for given screen
M.setup = function(s)
	require("ui.status_bar.top").setup(s)
	require("ui.status_bar.bottom").setup(s)
	require("ui.status_bar.windows").setup(s)
end

return M
