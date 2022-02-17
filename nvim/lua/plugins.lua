		-- Disable Vim-Tmux navigator when zooming the Vim pane in Tmux
		variables.global.tmux_navigator_disable_when_zoomed = 1

		-- Asynctasks config
		-- Quickfix list height
		variables.global.asyncrun_open = 8

		-- lightspeed config
			vim.cmd "map <space> <plug>Lightspeed_,_ft"

	ultisnips-cmp

	-- AsyncTasks
	use {
		"skywind3000/asynctasks.vim",
		requires = "skywind3000/asyncrun.vim",
		config = function()
		end
	}

	-- Telescope
	use {
		"nvim-telescope/telescope.nvim",
		requires = {
			{ "fhill2/telescope-ultisnips.nvim" },
			{ "GustavoKatel/telescope-asynctasks.nvim" }
		},
		config = function()
			require "plugin-config.telescope-config"
		end
	}

	-- Todo management
	use {
		"AmeerTaweel/todo-comments.nvim",
		requires = "nvim-lua/plenary.nvim",
		config = function()
			require "plugin-config.todo"
		end
	}
