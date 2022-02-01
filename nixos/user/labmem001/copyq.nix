{ pkgs, ... }:

let
	copyqConfigPath = ../../../copyq;
in {
	home.packages = with pkgs; [
		copyq
	];

	xdg.configFile.copyqConfig = {
		source = copyqConfigPath;
		target = "copyq";
		recursive = true;
	};
}
