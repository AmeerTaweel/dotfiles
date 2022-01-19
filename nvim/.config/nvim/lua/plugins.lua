-- { Packer Package Manager Configuration }

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

	-- use "tpope/vim-fugitive"

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
		wants = "nvim-treesitter",
		config = function()
			require "plugin-config.ts.nvim-ts-rainbow"
		end
	}

	-- Set commentstring based on cursor location in file
	use {
		"JoosepAlviste/nvim-ts-context-commentstring",
		wants = "nvim-treesitter",
		config = function()
			require "plugin-config.ts.nvim-ts-context-commentstring"
		end
	}

	-- Auto close and rename HTML tags
	use {
		"windwp/nvim-ts-autotag",
		wants = "nvim-treesitter",
		config = function()
			require "plugin-config.ts.nvim-ts-autotag"
		end
	}

	-- Auto Pairs
	-- use {
	-- 	"windwp/nvim-autopairs",
	-- 	wants = "nvim-treesitter",
	-- 	config = function()
	-- 		require "plugin-config.ts.nvim-autopairs"
	-- 	end
	-- }

	-- Indentation Guides
	use {
		"lukas-reineke/indent-blankline.nvim",
		wants = "nvim-treesitter",
		config = function()
			require "plugin-config.ts.indent-blankline"
		end
	}

	-- TreeSitter Playground
	use {
		"nvim-treesitter/playground",
		wants = "nvim-treesitter"
	}

	-- { LSP }

	-- LSP default configuration
	use {
		"neovim/nvim-lspconfig",
		wants = "which-key.nvim"
	}

	-- Auto-install language servers
    use {
        "williamboman/nvim-lsp-installer",
        wants = "nvim-lspconfig",
        config = function()
            require "plugin-config.lsp.nvim-lspconfig"
        end
    }

	-- Auto completion
	-- use {
	-- 	"hrsh7th/nvim-compe",
	-- 	config = function()
	-- 		require "plugin-config.nvim-compe"
	-- 	end
	-- }

	-- { Navigation }

    -- Better Vim and Tmux navigation
    -- Should also installed in Tmux with Tmux Plugin Manager (TPM)
	use {
        "christoomey/vim-tmux-navigator",
		config = function()
			require "plugin-config.vim-tmux-navigator"
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

	use {
		"lervag/vimtex",
		config = function()
			variables.global.vimtex_view_method = "zathura"
		end
	}

	use {
		"andymass/vim-matchup",
		wants = "nvim-treesitter",
		config = function()
			require "plugin-config.vim-matchup"
		end
	}

	-- Vim Surround
	use { "tpope/vim-surround" }

	-- Vim Repeat
	use { "tpope/vim-repeat" }

	use {
		"nvim-telescope/telescope.nvim",
		requires = {
			{ "nvim-lua/plenary.nvim" },
			{ "nvim-telescope/telescope-fzf-native.nvim", run = "make" }
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
			-- vim.g.UltiSnipsExpandTrigger = "<plug>(ultisnips_expand)"
			vim.g.UltiSnipsExpandTrigger = "<tab>"
			-- vim.g.UltiSnipsJumpForwardTrigger = "<plug>(ultisnips_jump_forward)"
			vim.g.UltiSnipsJumpForwardTrigger = "<tab>"
			-- vim.g.UltiSnipsJumpBackwardTrigger = "<plug>(ultisnips_jump_backward)"
			vim.g.UltiSnipsJumpBackwardTrigger = "<s-tab>"
			vim.g.UltiSnipsListSnippets = "<c-x><c-s>"
			vim.g.UltiSnipsRemoveSelectModeMappings = 0
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

--[[
Language-Specific

JSON-C Support for VIM
use { 'kevinoid/vim-jsonc' }

Syntax highlighting for TMUX config files
use { 'tmux-plugins/vim-tmux' }

Syntax Highlighting for sxhkdrc
use { 'baskerville/vim-sxhkdrc' }

" Vim Airline: Vim statusbar
Plug 'vim-airline/vim-airline'

" ZoomWin: Zoom windows
Plug '~/.vim/plugged-manual/ZoomWin'

" Fuzzy Finder: File finder
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

" Vim Visual Multi: Multiple Cursors
Plug 'mg979/vim-visual-multi', {'branch': 'master'}

" Vim Targets: More text objects to operate on
Plug 'wellle/targets.vim'
" Cheatsheet
" https://github.com/wellle/targets.vim/blob/master/cheatsheet.md
--]]
