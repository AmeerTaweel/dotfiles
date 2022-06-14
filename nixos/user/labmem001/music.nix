{ user, pkgs, ... }:

{
	services.mpd = {
		enable = false;
		musicDirectory = user.xdgDirs.music;
		network.startWhenNeeded = true;
		extraConfig = ''
			audio_output {
				type "pulse"
				name "pulse"
			}
		'';
	};

	programs.ncmpcpp = {
		enable = true;
		mpdMusicDir = /.${user.xdgDirs.music};
		# bindings = [
		# 	{ key = "j"; command = "scroll_down"; }
		# 	{ key = "k"; command = "scroll_up"; }
		# 	{ key = "J"; command = [ "select_item" "scroll_down" ]; }
		# 	{ key = "K"; command = [ "select_item" "scroll_up" ]; }
		# ];
		settings = {
		};
	};

	home.packages = with pkgs; [
		mpc_cli
	];

	# MPDRIS2
	# playerctl
}
