{ host, pkgs, ... }:

{
	sound.enable = true;
	hardware.pulseaudio.enable = true;
	hardware.pulseaudio.package = pkgs.pulseaudioFull;
}
