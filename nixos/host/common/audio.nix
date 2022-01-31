{ host, pkgs, ... }:

{
	# Use PulseAudio
	# sound.enable = true;
	# hardware.pulseaudio.enable = true;
	# hardware.pulseaudio.package = pkgs.pulseaudioFull;

	# Use PipeWire
	security.rtkit.enable = true;
	services.pipewire = {
		enable = true;
		alsa.enable = true;
		alsa.support32Bit = true;
		pulse.enable = true;
		# Enable JACK applicaitons
		# jack.enable = true;

		# Bluetooth Configuration
		media-session.config.bluez-monitor.rules = [{
			# Match all cards
			matches = [ { "device.name" = "~bluez_card.*"; } ];
			actions = {
				"update-props" = {
					"bluez5.auto-connect" = [ "hfp_hf" "hsp_hs" "a2dp_sink" ];
					"bluez5.reconnect-profiles" = [ "hfp_hf" "hsp_hs" "a2dp_sink" ];
					# mSBC is not expected to work on all headset + adapter combinations.
					"bluez5.msbc-support" = true;
					# SBC-XQ is not expected to work on all headset + adapter combinations.
					"bluez5.sbc-xq-support" = true;
				};
			};
		} {
			matches = [
				# Match all sources
				{ "node.name" = "~bluez_input.*"; }
				# Match all outputs
				{ "node.name" = "~bluez_output.*"; }
			];
			actions = {
				"node.pause-on-idle" = false;
			};
		}];
	};
}
