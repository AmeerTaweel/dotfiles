-- Declare the vim global (for LSP)
_G.vim = vim

-- { Execution }

_G.exec = {}
-- Executes an ex-command
_G.exec.command = vim.api.nvim_command
-- Executes VimScript code
_G.exec.vimscript = vim.cmd
-- Invokes a Vim function or a user function
_G.fn = vim.fn

-- +------------+
-- | ## Options |
-- +------------+

_G.options = {}
_G.options.global = vim.go
_G.options.window = vim.wo
_G.options.buffer = vim.bo
_G.options.object = vim.opt

-- +--------------+
-- | ## Variables |
-- +--------------+

_G.variables = {}
_G.variables.global = vim.g

-- +----------------+
-- | ## Key Mapping |
-- +----------------+

_G.setKeymap = vim.api.nvim_set_keymap

-- ### Modes
_G.mode = {}
_G.mode.normal = "n"
_G.mode.visual = "v"
_G.mode.insert = "i"
_G.mode.select = "s"

-- ### Keys
_G.key = {}
_G.key.leader = "<leader>"
_G.key.tab = "<tab>"
_G.key.shiftTab = "<s-tab>"
_G.key.ctrlN = "<c-n>"
_G.key.ctrlP = "<c-p>"

-- +--------+
-- | ## Lua |
-- +--------+

-- ### List-Like Tables
_G.list = {}
_G.list.contains = vim.tbl_contains

-- ### Strings
-- _G.string is already defined in the language
_G.string.split = vim.split

-- +-----------+
-- | ## Others |
-- +-----------+

_G.replaceTermCodes = vim.api.nvim_replace_termcodes
