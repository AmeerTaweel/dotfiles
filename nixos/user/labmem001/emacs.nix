{ pkgs, ... }:

let
	emacsConfigurationPath = ../../../emacs;
in {
	programs.emacs = {
		enable = true;
		extraPackages = emacsPackages: with emacsPackages; [
			# Config Util
			load-relative
			use-package
			diminish

			# Evil Mode
			evil
			evil-collection
			evil-commentary

			# Keybindings
			which-key
			general
			hydra

			# Completion
			vertico
			marginalia
			orderless
			consult
			embark
			embark-consult
			corfu
			corfu-doc
			cape

			# Org Mode
			org-roam

			# Common Lisp
			sly
			sly-asdf
			sly-repl-ansi-color

			# Scheme
			geiser
			geiser-guile
			geiser-racket

			# Other
			rainbow-delimiters
			helpful
			hl-todo # TODO Highlight

			# PDF
			# TODO
			pdf-tools
		];
	};

	/* xdg.configFile.emacsConfiguration = { */
	/* 	source = emacsConfigurationPath; */
	/* 	target = "emacs"; */
	/* 	recursive = true; */
	/* }; */
}
