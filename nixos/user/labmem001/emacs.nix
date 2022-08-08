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
			evil-matchit
			evil-surround
			evil-snipe

			# Keybindings
			which-key
			general

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

			# Nix
			nix-mode

			# Other
			rainbow-delimiters
			helpful
			hl-todo # TODO Highlight
			pdf-tools
      magit
		];
	};

	/* xdg.configFile.emacsConfiguration = { */
	/* 	source = emacsConfigurationPath; */
	/* 	target = "emacs"; */
	/* 	recursive = true; */
	/* }; */
}
