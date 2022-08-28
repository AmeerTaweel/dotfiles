require "globals"

-- { Hybrid Line Numbers }

-- Relative line numbers
options.window.relativenumber = true

-- Show current line number
options.window.number = true

-- { Search }

-- Select items found in search
options.global.incsearch = true

-- Highlight searches by default
options.global.hlsearch = true

-- Ignore case when searching
options.global.ignorecase = true

-- Unless you type a capital
options.global.smartcase = true

-- { Indentation }

local tabWidth = 4

-- Enable indent
exec.command "filetype on"
exec.command "filetype indent on"
exec.command "filetype plugin on"

-- Enable auto-indentation
options.object.autoindent = true

-- Enable smart-indentation
options.object.smartindent = true

-- Use tabs not spaces
options.object.expandtab = false

-- Set tab width
options.object.shiftwidth = tabWidth
options.object.tabstop = tabWidth
options.object.softtabstop = 0 -- Use hard tabs always

-- { Temporary Files }

local temporaryFilesDirectory = "/tmp/nvim"
-- Make sure the directory /tmp/nvim exists
if fn.isdirectory(temporaryFilesDirectory) == 0 then
	fn.mkdir(temporaryFilesDirectory)
end

-- Save backup, swap, and undo files in the temporaryFilesDirectory.
-- This is cleaner than saving them in the current working directory.
options.global.backupdir = temporaryFilesDirectory
options.global.directory = temporaryFilesDirectory
options.global.undodir = temporaryFilesDirectory

-- Enable Persistent Undo
options.buffer.undofile = true

-- { Others }

-- Set characters-per-line-limit
options.buffer.textwidth = 80

-- Show column indicating the characters-per-line-recommended-limit
options.window.colorcolumn = "80"

-- Highlight cursor line and column
-- options.window.cursorline = true
-- options.window.cursorcolumn = true

-- Enable true color
options.global.termguicolors = true

-- Do not wrap lines
options.window.wrap = false

-- Always show status line for all windows
options.global.laststatus = 2

-- Confirm operations that would fail normally (like closing an unsaved buffer)
options.global.confirm = true

-- Allow backspacing over indention, line breaks and insertion start
options.global.backspace = "indent,eol,start"

-- Automatically re-read files if modified outside the editor
options.global.autoread = true

-- Allow leaving buffers without saving, leaving buffer in background
options.global.hidden = true

-- Enable mouse support for scrolling and resizing
options.global.mouse = "a"

-- Set the window’s title, reflecting the file currently being edited
options.global.title = true

-- Enable expanding commands with the tab key
options.global.wildmenu = true

-- Split to the right and down
options.global.splitright = true
options.global.splitbelow = true

-- Set the number of saved commands in history
options.global.history = 10000

-- Show incomplete commands at the bottom
options.global.showcmd = true

-- Ignore file’s mode lines
options.buffer.modeline = false

-- Always show sign column
options.window.signcolumn = "yes"

-- Use a block cursor in insert mode
options.global.guicursor = "i:block"

-- Auto-completion options
options.global.completeopt = "menuone,noinsert,noselect,preview"

-- Use diff mode vertical split
options.global.diffopt = "internal,filler,closeoff,vertical"

-- Remove the Netrw banner
variables.global.netrw_banner = 0

-- Give more space for displaying messages.
options.global.cmdheight = 1

-- Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
-- delays and poor user experience.
options.global.updatetime = 100

-- Time to wait for a keybinding to complete
options.global.timeoutlen = 500

-- Number of vertical context lines
options.global.scrolloff = 10
-- Number of horizontal context lines
options.global.sidescrolloff = 5

-- Don't pass messages to |insertion-completion-menu|.
options.global.shortmess = options.global.shortmess .. "c"

exec.command "set list"
local defaultListChars = {
	eol = "↲",
	trail = "·",
	space = ".",
	tab = "-->"
}
options.object.listchars = defaultListChars

options.object.viewoptions = {
	"folds",
	"cursor"
}

-- Use bash as the default shell.
-- options.global.shell = "/usr/bin/env bash"
