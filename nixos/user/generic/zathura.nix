# Zathura
#
# Highly customizable and functional document viewer focused on keyboard
# interaction.

{ ... }:

{
	programs.zathura = {
		enable  = true;
		options = {
			# Enable dark mode by default
			recolor = true;
			# Ability to paste selection
			selection-clipboard = "clipboard";
		};
	};
}
