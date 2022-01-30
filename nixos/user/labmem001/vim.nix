{ pkgs, ... }:

let
	vimConfigurationPath = ../../../vim;

	customVimPackages = {};

	customVimPackages.vim-window-swap = pkgs.vimUtils.buildVimPlugin {
		name = "vim-window-swap";
		src = pkgs.fetchFromGitHub {
			owner = "wesQ3";
			repo = "vim-windowswap";
			rev = "15db3f697aa1fa696d99fcdc920c90cd2cec855e";
			sha256 = "S1CN0Dbb2BtZx4Jhafac2AltkzV2sPAixSESBXbn7/8=";
		};
	};
in {
	programs.vim = {
		enable = true;
		extraConfig = builtins.readFile "${vimConfigurationPath}/vimrc";
		plugins = with pkgs.vimPlugins; [
			# Statusbar
			lightline-vim

			# Better Vim and Tmux navigation
			# NOTE: Should also be installed for Tmux
			vim-tmux-navigator

			# Swap split windows with ease
			customVimPackages.vim-window-swap

			# Git for Vim
			vim-fugitive

			# Unix shell commands in Vim
			vim-eunuch

			# Syntax and indentation support for many languages
			vim-polyglot

			# Comments for Vim
			vim-commentary

			# More text objects to operate on
			# NOTE: Cheatsheet for this plugin in the link below
			# https://github.com/wellle/targets.vim/blob/master/cheatsheet.md
			targets-vim

			# Qouting and parenthesizing made simple
			vim-surround

			# Enable repeating supported plugin maps with the "." operator
			vim-repeat
		];
	};

	home.file.vimConfiguration = {
		source = vimConfigurationPath;
		target = ".vim";
		recursive = true;
	};
}
