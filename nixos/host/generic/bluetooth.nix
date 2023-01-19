{ pkgs, ... }:

{
	hardware.bluetooth.enable = true;
	hardware.bluetooth.package = pkgs.bluez;

	services.blueman.enable = true;

	systemd.user.services.mpris-proxy = {
		description = "Control media player using Bluetooth headset buttons.";
		after = [ "network.target" "sound.target" ];
		script = "${pkgs.bluez}/bin/mpris-proxy";
		wantedBy = [ "default.target" ];
	};
}
