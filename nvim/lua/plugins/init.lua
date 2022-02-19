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

-- Tmux Navigator
-- Disable Vim-Tmux navigator when zooming the Vim pane in Tmux
variables.global.tmux_navigator_disable_when_zoomed = 1

-- Asynctasks
-- Quickfix list height
variables.global.asyncrun_open = 8

-- Lightspeed
vim.cmd "map <space> <plug>Lightspeed_,_ft"

-- todo.nvim
local todo = require "todo"
todo.setup { signs = { enable = false } }
