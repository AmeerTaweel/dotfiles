{ pkgs, vscode-extensions, ... }:

{
	programs.vscode = {
		enable = true;
		package = pkgs.vscodium-fhs;
		mutableExtensionsDir = false;
		extensions = with vscode-extensions; [
			# Theme
			Equinusocio.vsc-material-theme

			# Icon Theme
			PKief.material-icon-theme

			# Vim Emulation
			vscodevim.vim

			eamodio.gitlens
			yzhang.markdown-all-in-one
			DavidAnson.vscode-markdownlint
			ms-python.python
			ms-vscode-remote.remote-containers
		];
	};
}
