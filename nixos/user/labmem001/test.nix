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
	awesomewmConfigPath = ../../../awesomewm;
	ideaVimRCPath = ../../../idea-vim/ideavimrc;
	ideaVimConfigPath = ../../../idea-vim/config;
in {
	programs.home-manager.enable = true;
	# Use Bluetooth headset buttons to control media player???
	services.mpris-proxy.enable = true;
	/* home.stateVersion = "21.11"; */
	services.blueman-applet.enable = true;
	services.network-manager-applet.enable = true;
	# Automount removable media ????
	services.udiskie.enable = true;

	home.keyboard.options = [ "ctrl:nocaps" ];
	home.keyboard.layout = "us,ar";

	# Enable the X11 windowing system.
	xsession.enable = true;
	xsession.numlock.enable = true;

	xsession.windowManager.awesome = {
		enable = true;
		luaModules = [ pkgs.luaPackages.vicious ];
	};

	xdg.configFile.awesomewmConfig = {
		source = awesomewmConfigPath;
		target = "awesome";
		recursive = true;
	};

	xdg.configFile.awesomewmTheme = {
		text = "return require('ui.themes.${user.theme}.theme')";
		target = "awesome/ui/theme.lua";
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
	};

	programs.direnv = {
		enable = true;
		enableBashIntegration = true;
		# This is enabled by default
		# enableFishIntegration = true;
	};
	programs.direnv.nix-direnv.enable = true;

	programs.emacs = {
		enable = true;
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
		yt-dlp
		tartube-yt-dlp
		nuclear

		# IDE's
		# vscode
		# android-studio
		jetbrains.idea-ultimate

		# Other
		ffmpeg
		bat
		fd 
		ripgrep
		htop
		copyq
		rar
		obsidian
		gimp
		octaveFull # Gnu Octave
		libreoffice
		postman
		devour
		sshfs
		lookatme
		unityhub

		/* termpdfpy */
		/* exa */
		/* vifm */
		/* entr */
		/* texlive.combined.scheme-full */
		# neovim stuff
		/* unstable.pyright */
		/* unstable.yaml-language-server */
		/* unstable.texlab */
		/* unstable.ccls */
		/* unstable.rust-analyzer */
		/* unstable.rustfmt */
		/* unstable.nodePackages.bash-language-server */
		/* unstable.nodePackages.vim-language-server */
		/* unstable.nodePackages.vscode-langservers-extracted */
		/* nodePackages.typescript-language-server */
		/* nodePackages.diagnostic-languageserver */
		/* unstable.sumneko-lua-language-server */
		/* shellcheck */
		/* luaformatter */
		/* vim-vint */
		/* nodePackages.markdownlint-cli2 */
		#debug adapters
		/* vscode-extensions.ms-vscode.cpptools */
		# nodePackages.eslint
		# nodePackages.prettier
		# (unstable.nodePackages.typescript-language-server.overrideAttrs (oldAttrs: {
		# 	buildInputs = oldAttrs.buildInputs ++ [ unstable.nodePackages.typescript ];
  		# }))

		/* gcc11 */
		/* nodejs */
		/* wget */
		/* unzip */
		/* glibc */
		/* gnumake */

		# hplip-gui
		# not found
		# to:
		# latex
		# patched font
		# pulse secure vpn for university
	];

	home.file.ideaVimRC = {
		source = ideaVimRCPath;
		target = ".ideavimrc";
	};
	xdg.configFile.ideaVimConfig = {
		source = ideaVimConfigPath;
		target = "idea-vim";
		recursive = true;
	};
}
