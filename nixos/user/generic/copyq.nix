{ pkgs, ... }:

{
	home.packages = [ pkgs.copyq ];

	xdg.configFile.copyqConfig = {
		source = ../../../copyq;
		target = "copyq";
		recursive = true;
	};
}
