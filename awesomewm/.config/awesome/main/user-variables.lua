-- { User Variables }

-- Default terminal
local terminal = "alacritty"

-- Default text editor
local system_editor = os.getenv("EDITOR")
local fallback_editor = "vim"
local editor = system_editor or fallback_editor

-- Command to launch text editor
local editor_cmd = terminal .. " -e " .. editor

-- Mod4 is the Super key
local mod_key = "Mod4"

local M = {
	terminal = terminal,
	editor = editor,
	editor_cmd = editor_cmd,
	mod_key = mod_key
}

return M
