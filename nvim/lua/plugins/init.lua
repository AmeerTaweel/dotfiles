require "globals"

require "plugins.nvim-cmp"
require "plugins.telescope-config"
require "plugins.ultisnips"
require "plugins.vim-matchup"
require "plugins.which-key"

require "plugins.ts.nvim-treesitter"
require "plugins.ts.indent-blankline"
require "plugins.ts.autopairs"
require "plugins.ts.autotag"
require "plugins.ts.context-commentstring"
require "plugins.ts.rainbow"

require "plugins.lsp.nvim-lspconfig"

-- Asynctasks
-- Quickfix list height
variables.global.asyncrun_open = 8

-- Lightspeed
vim.cmd "map <space> <plug>Lightspeed_,_ft"

-- todo.nvim
local todo = require "todo"
todo.setup { signs = { enable = false } }

-- fidget.nvim
local fidget = require "fidget"
fidget.setup { }

-- Comment.nvim
local comment = require("Comment")
comment.setup { }

-- taboo.vim
-- Show tab numbers
variables.global.taboo_tab_format = " [%N] %f%m "
variables.global.taboo_renamed_tab_format = " [%N] %l%m "
