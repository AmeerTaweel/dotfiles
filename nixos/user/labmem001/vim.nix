{ pkgs, ... }:

let
	vimConfigurationPath = ../../../vim;
in {
	programs.vim = {
		enable = true;
		extraConfig = builtins.readFile "${vimConfigurationPath}/vimrc";
		plugins = with pkgs.vimPlugins; [
			# Statusbar
			lightline-vim

			# Better Vim and Kitty navigation
			# NOTE: Should also be installed for Kitty
			vim-kitty-navigator

			# Swap split windows with ease
			vim-windowswap

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
