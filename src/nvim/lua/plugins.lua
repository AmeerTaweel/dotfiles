--[[
+----------------------------------------+
| # Packer Package Manager Configuration |
+----------------------------------------+
--]]

require "globals"

-- +--------------------------------------------+
-- | ## Install Packer if not already installed |
-- +--------------------------------------------+

-- The "git" command must be installed on the system for this to work

local packerInstallPath = fn.stdpath("data").."/site/pack/packer/start/packer.nvim"
local packerRepoURL = "https://github.com/wbthomason/packer.nvim"

if fn.empty(fn.glob(packerInstallPath)) == 1 then
	fn.system({"git", "clone", packerRepoURL, packerInstallPath})
	exec.command "packadd packer.nvim"
end

-- +------------------+
-- | ## Packer Config |
-- +------------------+

local packer = require "packer"
local packerUtils = require "packer.util"

-- Define packages
local packerPackages = function(use)

	-- Packer can manage itself
	use { "wbthomason/packer.nvim" }

	-- [[ Themes ]]

	-- Darcula
	use { "doums/darcula" }

	-- Ayu
	use { "Shatur/neovim-ayu" }

	-- OneHalf
	use { "sonph/onehalf", rtp = "vim" }

	-- Gruvbox
	use "gruvbox-community/gruvbox"

	-- Dracula
	use { "dracula/vim", as = "dracula" }

	-- Nord
	use "arcticicestudio/nord-vim"

	-- [[ Git ]]

	-- Git for Vim
	use "tpope/vim-fugitive"

	-- [[ TreeSitter ]]

	-- TreeSitter
	use {
		"nvim-treesitter/nvim-treesitter",
		run = ":TSUpdate",
		config = function()
			require "plugin-config.ts.nvim-treesitter"
		end
	}

	-- TreeSitter raibow parentheses pairs
	use {
		"p00f/nvim-ts-rainbow",
		wants = "nvim-treesitter",
		config = function()
			require "plugin-config.ts.nvim-ts-rainbow"
		end
	}

	-- Set the commentstring based on cursor location in file
	use {
		"JoosepAlviste/nvim-ts-context-commentstring",
		wants = "nvim-treesitter",
		config = function()
			require "plugin-config.ts.nvim-ts-context-commentstring"
		end
	}

	-- Use treesitter to auto close and auto rename HTML tags
	use {
		"windwp/nvim-ts-autotag",
		wants = "nvim-treesitter",
		config = function()
			require "plugin-config.ts.nvim-ts-autotag"
		end
	}

	use {
		"windwp/nvim-autopairs",
		wants = "nvim-treesitter",
		config = function()
			require "plugin-config.ts.nvim-autopairs"
		end
	}

	use {
		"lukas-reineke/indent-blankline.nvim",
		wants = "nvim-treesitter",
		config = function()
			require "plugin-config.ts.indent-blankline"
		end
	}

	use {
		"nvim-treesitter/playground",
		wants = "nvim-treesitter"
	}

	use {
		"nvim-telescope/telescope.nvim",
		requires = {
			{"nvim-lua/popup.nvim"},
			{"nvim-lua/plenary.nvim"},
			-- Telescope extensions
			{"nvim-telescope/telescope-fzy-native.nvim"}
		},
		config = function()
			require "plugin-config.telescope-config"
		end
	}

	-- [[ LSP ]]

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
	use {
		"hrsh7th/nvim-compe",
		config = function()
			require "plugin-config.nvim-compe"
		end
	}

	-- [[ Others ]]

    -- Better Vim and Tmux navigation
    -- Should also installed in TMUX with Tmux Plugin Manager (TPM)
	use {
        "christoomey/vim-tmux-navigator",
		config = function()
			require "plugin-config.vim-tmux-navigator"
		end
	}

	-- Highlight yanked region
	use "machakann/vim-highlightedyank"

    -- Unix shell commands in Vim
    use "tpope/vim-eunuch"

    -- Swap split windows with ease
    use {
        "wesQ3/vim-windowswap",
		config = function()
			require "plugin-config.window-swap"
		end
    }

	-- Comments for Vim
	use "tpope/vim-commentary"

	-- Display a popup with possible keybindings completions
	use {
		"folke/which-key.nvim",
		config = function()
			require "plugin-config.which-key"
		end
	}

	use {
		"kyazdani42/nvim-web-devicons",
		config = function()
			require "plugin-config.icons"
		end
	}

	use {
		"kyazdani42/nvim-tree.lua",
		requires = "kyazdani42/nvim-web-devicons",
		config = function()
			require "plugin-config.file-tree"
		end
	}
end

-- Define configurations
local packerGlobalConfig = {
	display = {
		open_fn = packerUtils.float
	}
}

-- Pass packages and configuration to Packer
packer.startup({ packerPackages, config = packerGlobalConfig })

-- Auto install missing packages automatically
exec.command "PackerInstall"

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

" Vim Surround: Quoting and parenthesizing made simple
Plug 'tpope/vim-surround'

" Vim Repeat: Enable repeating supported plugin maps with "."
Plug 'tpope/vim-repeat'

--]]
