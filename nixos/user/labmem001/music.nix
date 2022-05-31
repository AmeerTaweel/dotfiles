{ pkgs, ... }:

let
	musicDirectory = ../../../vim;
in {
	services.mpd = {
		enable = true;
		musicDirectory = "$XDG_MUSIC_DIR";
		extraConfig = "";
	}
}
