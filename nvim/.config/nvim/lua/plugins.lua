require "globals"

-- {{ Install Packer if not already installed }}

-- Dependencies: git

local install_path = fn.stdpath("data").."/site/pack/packer/start/packer.nvim"
local repo_url = "https://github.com/wbthomason/packer.nvim"
local is_first_install = false

if fn.empty(fn.glob(install_path)) > 0 then
	fn.system({ "git", "clone", repo_url, install_path })
	is_first_install = fn.system({"git", "clone", "--depth", "1", repo_url, install_path})
end

-- {{ Packer Config }}

local packer = require "packer"
local packer_utils = require "packer.util"

local packages = function(use)

	-- Packer can manage itself
	use { "wbthomason/packer.nvim" }

	-- { Themes }

	-- Darcula
	use { "doums/darcula" }

	-- Ayu
	use { "Shatur/neovim-ayu" }

	-- OneHalf
	use { "sonph/onehalf", rtp = "vim" }

	-- Gruvbox
	use { "gruvbox-community/gruvbox" }

	-- Dracula
	use { "dracula/vim", as = "dracula" }

	-- Nord
	use { "arcticicestudio/nord-vim" }

	-- { Version Control }

	use {
		"TimUntersberger/neogit",
		requires = "nvim-lua/plenary.nvim"
	}

	-- { TreeSitter }

	-- TreeSitter
	use {
		"nvim-treesitter/nvim-treesitter",
		run = ":TSUpdate",
		config = function()
			require "plugin-config.ts.nvim-treesitter"
		end
	}

	-- Raibow Pairs
	use {
		"p00f/nvim-ts-rainbow",
		requires = "nvim-treesitter",
		config = function()
			require "plugin-config.ts.nvim-ts-rainbow"
		end
	}

	-- Set commentstring based on cursor location in file
	use {
		"JoosepAlviste/nvim-ts-context-commentstring",
		requires = "nvim-treesitter",
		config = function()
			require "plugin-config.ts.nvim-ts-context-commentstring"
		end
	}

	-- Auto close and rename HTML tags
	use {
		"windwp/nvim-ts-autotag",
		requires = "nvim-treesitter",
		config = function()
			require "plugin-config.ts.nvim-ts-autotag"
		end
	}

	-- Auto Pairs
	use {
		"windwp/nvim-autopairs",
		requires = "nvim-treesitter",
		config = function()
			require "plugin-config.ts.nvim-autopairs"
		end
	}

	-- Indentation Guides
	use {
		"lukas-reineke/indent-blankline.nvim",
		requires = "nvim-treesitter",
		config = function()
			require "plugin-config.ts.indent-blankline"
		end
	}

	-- TreeSitter Playground
	use {
		"nvim-treesitter/playground",
		requires = "nvim-treesitter"
	}

	-- { LSP }

	-- LSP default configuration
	use {
		"neovim/nvim-lspconfig",
		requires = "hrsh7th/nvim-cmp",
		config = function()
			require "plugin-config.lsp.nvim-lspconfig"
		end
	}

	-- Auto completion
	use {
		"hrsh7th/nvim-cmp",
		requires = {
			{ "hrsh7th/cmp-nvim-lsp" },
			{ "hrsh7th/cmp-path" },
			{ "hrsh7th/cmp-cmdline" },
			{ "hrsh7th/cmp-buffer" },
			{ "hrsh7th/cmp-nvim-lua" },
			{ "quangnguyen30192/cmp-nvim-ultisnips" }
		},
		config = function()
			require "plugin-config.nvim-cmp"
		end
	}

	-- { Navigation }

	-- Better Vim and Tmux navigation
	-- Should also installed in Tmux with Tmux Plugin Manager (TPM)
	use {
		"christoomey/vim-tmux-navigator",
		config = function()
			-- Disable Vim-Tmux navigator when zooming the Vim pane in Tmux
			variables.global.tmux_navigator_disable_when_zoomed = 1
		end
	}

	-- { Others }

	-- Highlight yanked region
	use { "machakann/vim-highlightedyank" }

	-- Unix shell commands in Vim
	use { "tpope/vim-eunuch" }

	-- Window move and swap
	use { "sindrets/winshift.nvim" }

	-- Comments for Vim
	use { "tpope/vim-commentary" }

	-- Display a popup with possible keybindings completions
	use {
		"folke/which-key.nvim",
		config = function()
			require "plugin-config.which-key"
		end
	}

	-- Improved % operator
	use {
		"andymass/vim-matchup",
		requires = "nvim-treesitter",
		config = function()
			require "plugin-config.vim-matchup"
		end
	}

	-- Vim Surround
	use { "tpope/vim-surround" }

	-- Vim Repeat
	use { "tpope/vim-repeat" }

	-- AsyncTasks
	use {
		"skywind3000/asynctasks.vim",
		requires = "skywind3000/asyncrun.vim",
		config = function()
			-- Quickfix list height
			variables.global.asyncrun_open = 8
		end
	}

	-- Telescope
	-- Dependencies: fd, rg
	use {
		"nvim-telescope/telescope.nvim",
		requires = {
			{ "nvim-lua/plenary.nvim" },
			{ "nvim-telescope/telescope-fzf-native.nvim", run = "make" },
			{ "fhill2/telescope-ultisnips.nvim" },
			{ "GustavoKatel/telescope-asynctasks.nvim" }
		},
		config = function()
			require "plugin-config.telescope-config"
		end
	}

	-- StatusLine
	use {
		"nvim-lualine/lualine.nvim",
		requires = { "kyazdani42/nvim-web-devicons", opt = true }
	}

	-- Text Alignment
	use { "godlygeek/tabular" }

	-- Snippets Engine
	use {
		"SirVer/ultisnips",
		config = function()
			require "plugin-config.ultisnips"
		end
	}

	-- Movement Operator
	use {
		"ggandor/lightspeed.nvim",
		requires = "tpope/vim-repeat",
		config = function()
			vim.cmd "map <space> <plug>Lightspeed_,_ft"
		end
	}

	-- Vim Targets: More text objects to operate on
	-- Cheatsheet
	-- https://github.com/wellle/targets.vim/blob/master/cheatsheet.md
	use { "wellle/targets.vim" }

	-- Todo management
	use {
		"AmeerTaweel/todo-comments.nvim",
		requires = "nvim-lua/plenary.nvim",
		config = function()
			require "plugin-config.todo"
		end
	}

	-- LaTeX Filetype Plugin
	-- Dependencies: texlive
	use {
		"lervag/vimtex",
		ft = { "tex" },
		config = function()
			variables.global.vimtex_view_method = "zathura"
		end
	}

	if is_first_install then
		packer.sync()
	end
end

-- Define configurations
local config = {
	display = {
		open_fn = packer_utils.float
	}
}

-- Pass packages and configuration to Packer
packer.startup({ packages, config = config })
