{ user, pkgs, config, ... }:

let
	mpdDataDir = "${config.xdg.dataHome}/mpd";
in {
	# TODO: configure playerctl and sxhkd
	home.packages = with pkgs; [
		mpd
		mpdris2 # control mpd via playerctl
		mpc_cli
	];

	services.playerctld.enable = true;

	xdg.configFile.mpdConfiguration = {
		text = ''
			# music location
			music_directory "${config.xdg.userDirs.music}"

			# database location
			db_file "${mpdDataDir}/tag_cache"

			# playlists location
			playlist_directory "${mpdDataDir}/playlists"

			# process id of mpd
			# pid_file "XXX"

			# mpd state location
			# state_file "XXX"

			# dynamic music information
			# sticker_file "XXX"

			# refresh database when files in music directory change
			auto_update "yes"

			# audio config
			audio_output {
				type "pulse"
				name "pulse"
			}
		'';
		target = "mpd/mpd.conf";
	};

	programs.ncmpcpp = {
		enable = true;
		mpdMusicDir = /.${config.xdg.userDirs.music};
		bindings = [
			# Vim movement keys
			{ key = "h"; command = "previous_column"; }
			{ key = "j"; command = "scroll_down"; }
			{ key = "k"; command = "scroll_up"; }
			{ key = "l"; command = "next_column"; }

			{ key = "J"; command = [ "select_item" "scroll_down" ]; }
			{ key = "K"; command = [ "select_item" "scroll_up" ]; }
		];
		settings = {
			# Use alternative interface
			user_interface = "alternative";
			# Display songs consistently
			playlist_editor_display_mode = "columns";
			search_engine_display_mode = "columns";
			browser_display_mode = "columns";
			playlist_display_mode = "columns";
			song_columns_list_format = "(30)[yellow]{a} (30)[white]{t} (30)[cyan]{b} (10)[red]{l}";
		};
	};
}
