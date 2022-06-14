{ host, user, ... }:

{
	imports = [
		{ _module.args = { inherit host user; }; }
		./general.nix
		./vim.nix
		./nvim.nix
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
