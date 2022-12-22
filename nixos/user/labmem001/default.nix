{ host, user, flake-inputs, ... }:

{
	imports = [
		{ _module.args = { inherit host user flake-inputs; }; }
		../generic/copyq.nix
		../generic/git.nix
		../generic/tmux.nix

		./general.nix
		./vim.nix
		./nvim.nix
		./vscode.nix
		./shell.nix
		./terminal.nix
		./xdg.nix
		./rofi.nix
		./sxhkd.nix
		./emacs.nix
		./music.nix
	];
}
