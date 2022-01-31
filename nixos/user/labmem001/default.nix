{ user, ... }:

{
	imports = [
		{ _module.args = { inherit user; }; }
		./test.nix
		./vim.nix
		./git.nix
        ./shell.nix
		./terminal.nix
		./xdg.nix
	];
}
