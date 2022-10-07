{ pkgs, ... }:

{
	fonts.fonts = with pkgs; [
		# Nerd Fonts
		(nerdfonts.override { fonts = [ "Hack" ]; })
	];
}
