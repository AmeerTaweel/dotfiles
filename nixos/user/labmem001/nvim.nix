{ user, pkgs, ... }:

let
	nvimConfigurationPath = ../../../nvim;
	
	nvimThemeConfiguration = {
		ayu-dark = ''
			vim.go.background = "dark"
			require("ayu").colorscheme()
		'';
		ayu-mirage = ''
			vim.go.background = "dark"
			require("ayu").setup({ mirage = true })
			require("ayu").colorscheme()
		'';
		ayu-light = ''
			vim.go.background = "light"
			require("ayu").colorscheme()
		'';
	};
in {
	programs.neovim = {
		enable = true;
		# extraConfig = builtins.readFile "${nvimConfigurationPath}/vimrc";
		plugins = with pkgs.vimPlugins; [
			plenary-nvim # Required by many packages

			# Themes
			neovim-ayu
			nord-nvim
			dracula-vim
			gruvbox-nvim
			onehalf

			# Version Control
			neogit

			# TreeSitter
			(nvim-treesitter.withPlugins (_: pkgs.tree-sitter.allGrammars))

			nvim-ts-rainbow
			nvim-ts-context-commentstring
			nvim-ts-autotag
			nvim-autopairs
			indent-blankline-nvim
			playground

			nvim-lspconfig
			nvim-cmp
			cmp-nvim-lsp
			cmp-path
			cmp-cmdline
			cmp-buffer
			cmp-nvim-lua

			# Navigation
			vim-tmux-navigator

			# Telescope
			telescope-nvim
			telescope-fzf-native-nvim

			vim-commentary
			vim-eunuch
			vim-surround
			vim-repeat
			vim-highlightedyank
			which-key-nvim
			vim-matchup
			lualine-nvim
			nvim-web-devicons
			tabular
			ultisnips
			lightspeed-nvim
			winshift-nvim
			asyncrun-vim
			asynctasks-vim

			# NOTE: Cheatsheet for targets-vim in the link below
			# https://github.com/wellle/targets.vim/blob/master/cheatsheet.md
			targets-vim
		];
		extraPackages = with pkgs; [
			# Telescope
			ripgrep
			fd

			# Language Servers
			rnix-lsp
		];
		extraConfig = ''
			luafile ~/.config/nvim/lua/settings.lua
			luafile ~/.config/nvim/lua/theme.lua
		'';
	};
	
	xdg.configFile.nvimConfiguration = {
		source = "${nvimConfigurationPath}/lua";
		target = "nvim/lua";
		recursive = true;
	};

	xdg.configFile.nvimTheme = {
		text = nvimThemeConfiguration.${user.theme};
		target = "nvim/lua/theme.lua";
	};

	xdg.configFile.nvimUltiSnips = {
		source = "${nvimConfigurationPath}/ulti-snips";
		target = "nvim/UltiSnips";
		recursive = true;
	};

	xdg.configFile.nvimAsyncTasks = {
		source = "${nvimConfigurationPath}/tasks.ini";
		target = "nvim/tasks.ini";
	};
}
