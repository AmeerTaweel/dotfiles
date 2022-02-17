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

-- { Options }

_G.options = {}
_G.options.global = vim.go
_G.options.window = vim.wo
_G.options.buffer = vim.bo
_G.options.object = vim.opt

-- { Variables }

_G.variables = {}
_G.variables.global = vim.g
_G.variables.environment = vim.env
