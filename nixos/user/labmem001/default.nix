{ host, user, vscode-extensions, ... }:

{
	imports = [
		{ _module.args = { inherit host user vscode-extensions; }; }
		./general.nix
		./vim.nix
		./nvim.nix
		./vscode.nix
		./git.nix
        ./shell.nix
		./terminal.nix
		./xdg.nix
		./rofi.nix
		./sxhkd.nix
		./copyq.nix
		./tmux.nix
		./emacs.nix
		./music.nix
	];
}
