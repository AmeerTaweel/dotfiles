{ pkgs, ... }:

let
	nvimConfigurationPath = ../../../nvim;
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

			# NOTE: Cheatsheet for targets-vim in the link below
			# https://github.com/wellle/targets.vim/blob/master/cheatsheet.md
			targets-vim
		];
	};


	xdg.configFile.nvimConfiguration = {
		source = nvimConfigurationPath;
		target = "nvim";
		recursive = true;
	};
}

