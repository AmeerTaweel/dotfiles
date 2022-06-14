{ pkgs, ... }:

let
	emacsConfigurationPath = ../../../emacs;
in {
	programs.emacs = {
		enable = true;
		extraPackages = emacsPackages: with emacsPackages; [
			# Util
			load-relative

			# Themes
			ayu-theme

			# Package Management
			use-package
			diminish

			# Evil Mode
			evil
			evil-collection

			# Modeline
			telephone-line

			# Keybindings
			which-key
			general
			hydra

			ivy
			counsel
			swiper
			ivy-rich

			rainbow-delimiters

			helpful

			doom-themes

			# Common Lisp
			slime

			# PDF
			pdf-tools

			# Music
			emms

			# TODO Highlight
			hl-todo
		];
	};

	/* xdg.configFile.emacsConfiguration = { */
	/* 	source = emacsConfigurationPath; */
	/* 	target = "emacs"; */
	/* 	recursive = true; */
	/* }; */
}
