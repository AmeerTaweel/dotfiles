{ pkgs, ... }:

{
	programs.qutebrowser = {
		enable = true;
		settings = {
			auto_save.session = true;
		};
	};
}
