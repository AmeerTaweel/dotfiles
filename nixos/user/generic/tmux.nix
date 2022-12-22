{ pkgs, ... }:

{
	programs.tmux.enable = true;

	home.file.tmuxRC = {
		source = ../../../tmux/rc.tmux;
		target = ".tmux.conf";
	};

	xdg.configFile.tmuxConfig = {
		source = ../../../tmux/config;
		target = "tmux/config";
		recursive = true;
	};
}
