{ host, user, pkgs, ... }:

let
	editors = {
		vim = {
			package = "${pkgs.vim}/bin/vim";
			manpager = "vim -M +MANPAGER -";
		};
		nvim = {
			package = "${pkgs.neovim}/bin/nvim";
			manpager = "nvim +Man!";
		};
	};
	editor = editors.${user.editor};

	power-menu = pkgs.writeShellScriptBin "power-menu" ''
		choices=(suspend lock hibernate reboot poweroff)

		declare -A actions
		actions[suspend]="systemctl suspend"
		actions[lock]="physlock"
		actions[hibernate]="systemctl hibernate"
		actions[reboot]="systemctl reboot"
		actions[poweroff]="systemctl poweroff"

		# Ask for confirmation for actions that are irreversible
		confirmations=(reboot poweroff)

		choice=$(echo -e "''${choices[@]}" | ${pkgs.rofi}/bin/rofi -sep " " -dmenu -p "power menu" -l 5)

		for i in "''${confirmations[@]}"; do
			if [[ $i == $choice ]]; then
				confirmation=$(echo -e "no\nyes" | ${pkgs.rofi}/bin/rofi -dmenu -p "are you sure you want to ''${choice}" -l 5)
				if [[ $confirmation != "yes" ]]; then
					exit
				fi
			fi
		done

		eval "''${actions[$choice]}"
	'';
in {
	programs.home-manager.enable = true;
	# Use Bluetooth headset buttons to control media player
	services.mpris-proxy.enable = true;
	services.blueman-applet.enable = true;
	services.network-manager-applet.enable = true;
	# Automount removable media
	services.udiskie.enable = true;

	home.keyboard.options = [ "ctrl:nocaps" ];
	home.keyboard.layout = "us,ar,tr";

	# Enable the X11 windowing system.
	xsession.enable = true;
	xsession.numlock.enable = true;

	xsession.windowManager.awesome = {
		enable = false;
		luaModules = [ pkgs.luaPackages.vicious ];
	};

	xsession.windowManager.xmonad.enable = true;

	xdg.configFile.awesomewmConfig = {
		source = ../../../awesomewm;
		target = "awesome";
		recursive = true;
	};

	xdg.configFile.awesomewmTheme = {
		text = "return require('ui.themes.${user.theme}.theme')";
		target = "awesome/ui/theme.lua";
	};

	xdg.configFile.discordConfig = {
		text = ''{ "SKIP_HOST_UPDATE": true }'';
		target = "discord/settings.json";
	};

	programs.nnn = {
		enable = true;
		package = pkgs.nnn.override ({ withNerdIcons = true; });
	};

	home.sessionVariables = {
		EDITOR = editor.package;
		MANPAGER = editor.manpager;
	};

	programs.mpv = {
		enable = true;
		defaultProfiles = [ "gpu-hq" ];
		config = {
			no-keepaspect-window = "";
			cache = "yes";
			video-sync = "display-resample";
			vo = "gpu";
			hwdec = "auto";
		};
		scripts = with pkgs.mpvScripts; [ mpris ];
	};

	programs.direnv = {
		enable = true;
		enableBashIntegration = true;
		# This is enabled by default
		# enableFishIntegration = true;
		nix-direnv.enable = true;
	};

	services.redshift = {
		enable = true;
		latitude = host.latitude;
		longitude = host.longitude;
		tray = true;
	};

	home.packages = with pkgs; [
		# Browsers
		google-chrome
		firefox
		opera
		qutebrowser
		vieb
		luakit
		tor-browser-bundle-bin

		# Communication
		discord
		element-desktop
		slack
		tdesktop
		zoom-us

		# Help
		tealdeer # TLDR
		cht-sh

		# Media
		vlc
		zathura
		okular
		koreader
		yt-dlp
		kdenlive
		image-roll

		# IDE's
		# android-studio
		jetbrains.idea-ultimate
		jetbrains.rider

		# Chess
		stockfish
		gnuchess
		chessx

		# Other
		stow
		ffmpeg
		imagemagick
		bat
		fd 
		ripgrep
		htop
		btop
		copyq
		rar
		zip
		unzip
		gimp
		octaveFull # Gnu Octave
		libreoffice
		postman
		devour
		sshfs
		unityhub
		gnome.nautilus
		alsa-utils
		playerctl

		file
		tree
		curl	
		power-menu

		# vifm
		# entr
		# hplip-gui
	];

	home.file.ideaVimRC = {
		source = ../../../idea-vim/ideavimrc;
		target = ".ideavimrc";
	};
	xdg.configFile.ideaVimConfig = {
		source = ../../../idea-vim/config;
		target = "idea-vim";
		recursive = true;
	};
	xdg.configFile.botpConfig = {
		source = ../../../btop;
		target = "btop";
		recursive = true;
	};

	programs.eww.enable = true;
	programs.eww.configDir = ../../../eww;
	programs.obs-studio.enable = true;
}
