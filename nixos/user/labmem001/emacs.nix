{ pkgs, ... }:

let
	emacsConfigurationPath = ../../../emacs;
in {
	programs.emacs = {
		enable = true;
		extraPackages = emacsPackages: with emacsPackages; [
			# Package Management
			use-package
			diminish

			# Evil Mode
			evil
			evil-collection

			# Modeline
			telephone-line

			ivy
			counsel
			swiper
			ivy-rich

			rainbow-delimiters

			which-key

			helpful

			doom-themes

			# Common Lisp
			slime
		];
	};

	xdg.configFile.emacsConfiguration = {
		source = emacsConfigurationPath;
		target = "emacs";
		recursive = true;
	};
}
