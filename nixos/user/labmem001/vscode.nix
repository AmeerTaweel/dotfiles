{ pkgs, ... }:

with pkgs.vscode-marketplace; let
	vscodeExtensions = with vscode; [
		# Theme
		teabyii.ayu

		# Icon Theme
		pkief.material-icon-theme

		# Vim Emulation
		vscodevim.vim

		# Git
		eamodio.gitlens

		# Language-Specific

		## Nix
		jnoortheen.nix-ide
	];

	openVSXExtensions = with open-vsx; [ ];
 
in {
	programs.vscode = {
		enable = true;
		package = pkgs.vscodium-fhs;
		mutableExtensionsDir = false;
		extensions = vscodeExtensions ++ openVSXExtensions;
		userSettings = {
			"editor.cursorStyle" = "block";
			"editor.renderWhitespace" = "trailing";
			"editor.minimap.enabled" = false;
			"files.autoSave" = "afterDelay";
			"gitlens.hovers.currentLine.over" = "line";
			"workbench.colorTheme" = "Ayu Dark";
			"workbench.iconTheme" = "material-icon-theme";
		};
	};
}
