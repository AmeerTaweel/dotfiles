{ host, user, ... }:

{
	imports = [
		{ _module.args = { inherit host user; }; }
		./test.nix
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
	];
}
