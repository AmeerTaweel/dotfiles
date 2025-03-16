-- Configure Telescope

require('telescope').setup {
   defaults = {
      sorting_strategy = 'ascending',
      layout_config = {
         prompt_position = 'top',
      },
   },
   extensions = {
      fzf = {},
   },
}
require('telescope').load_extension('fzf')

-- Keybindings

local wk = require('which-key')

wk.add({
   mode    = 'n',
   noremap = true,
   { '<leader>f', group = 'find (telescope)' },
   { '<leader>fb', '<cmd>Telescope buffers<cr>',                   desc = 'find buffer'             },
   { '<leader>fc', '<cmd>Telescope commands<cr>',                  desc = 'find command'            },
   { '<leader>fC', '<cmd>Telescope command_history<cr>',           desc = 'find command in history' },
   { '<leader>ff', '<cmd>Telescope find_files<cr>',                desc = 'find file'               },
   { '<leader>fh', '<cmd>Telescope help_tags<cr>',                 desc = 'find help'               },
   { '<leader>fs', '<cmd>Telescope current_buffer_fuzzy_find<cr>', desc = 'find line in buffer'     },
   { '<leader>fS', '<cmd>Telescope search_history<cr>',            desc = 'search search history'   },
   { '<leader>fg', '<cmd>Telescope live_grep<cr>',                 desc = 'find line in cwd'        },
   { '<leader>fd', '<cmd>Telescope diagnostics<cr>',               desc = 'find diagnostics'        },
})
