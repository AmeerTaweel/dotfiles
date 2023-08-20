{pkgs, ...}: let
  # emacsConfigurationPath = ../../../emacs;
in {
  # programs.emacs = {
  #   enable = true;
  # };

  programs.doom-emacs = {
    enable = true;
    doomPrivateDir = ./config/doom; # Directory containing your config.el, init.el
                                # and packages.el files
  };

  # programs.emacs = {
  # 	enable = true;
  # 	extraPackages = emacsPackages: with emacsPackages; [
  # 		# Config Util
  # 		load-relative
  # 		use-package
  # 		diminish
  #
  # 		# Evil Mode
  # 		evil
  # 		evil-collection
  # 		evil-commentary
  # 		evil-matchit
  # 		evil-surround
  # 		evil-snipe
  #
  # 		# Keybindings
  # 		which-key
  # 		general
  #
  # 		# Completion
  # 		vertico
  # 		marginalia
  # 		orderless
  # 		consult
  # 		embark
  # 		embark-consult
  # 		corfu
  # 		corfu-doc
  # 		cape
  #
  # 		# Org Mode
  # 		org-roam
  # 		doct
  #
  # 		# Programming
  # 		nix-mode
  # 		zig-mode
  #
  # 		## Common Lisp
  # 		sly
  # 		sly-asdf
  # 		sly-repl-ansi-color
  #
  # 		## Scheme
  # 		geiser
  # 		geiser-guile
  # 		geiser-racket
  #
  # 		## Treesitter
  # 		tree-sitter
  # 		tree-sitter-langs
  # 		tsc
  #
  # 		# Other
  # 		rainbow-delimiters
  # 		helpful
  # 		hl-todo # TODO Highlight
  # 		pdf-tools
  # 		magit
  # 		vterm
  # 	];
  # };

  /*
  xdg.configFile.emacsConfiguration = {
  */
  /*
  source = emacsConfigurationPath;
  */
  /*
  target = "emacs";
  */
  /*
  recursive = true;
  */
  /*
  };
  */
}
