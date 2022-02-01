{ user, pkgs, ... }:

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


	# Enable the X11 windowing system.
	xsession.enable = true;
	xsession.numlock.enable = true;

	# Enable the GNOME Desktop Environment.
	xsession.windowManager.awesome.enable = true;
	
	# Configure keymap in X11
	# xsession.layout = "us";
	# xsession.xkbOptions = "eurosign:e";

	# Some programs need SUID wrappers, can be configured further or are
	# started in user sessions.
	# programs.mtr.enable = true;
	programs.gpg = {
	  enable = true;
	  # enableSSHSupport = true;
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

	home.packages = with pkgs; [
		# Browsers
		google-chrome
		firefox

		# Communication
		discord
		slack
		tdesktop
		zoom-us

		# Help
		tealdeer # TLDR
		cht-sh

		# Media Players
		vlc

		/* emacs */
		/* vscode */
		# android-studio
		/* jetbrains.idea-ultimate */
		# Terminal Emulators
		/* kitty */
		# Version Control
		/* git */
		# Browsers
		/* qutebrowser */
		# X Server
		/* numlockx */
		/* xorg.xmodmap */
		/* xdotool */
		/* xclip */
		/* xdg-utils */
		/* xe */
		/* ripgrep */
		# Others
		/* postman */
		/* gnupg */
		/* pinentry */
		/* pass */
		/* stow */
		/* copyq */
		/* maim */
		/* rofi */
		/* tmux */
		/* exa */
		/* htop */
		/* nnn */
		/* vifm */
		/* entr */
		/* zathura */
		/* termpdfpy */
		/* gimp */
		/* octaveFull # Gnu octave */
		# steam
		/* libreoffice */
		# Browser
		/* firefox */
		/* opera */
		/* bat */
		/* fd */
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

		/* brightnessctl */
		# hplip-gui
		# failed
		# teamviewer
		# steam
		# not found
		# lookatme
		# to:
		# latex
		# patched font
		# ffmpeg
		# pulse secure vpn for university
		# hibernate issue
		# time issue with windows
		# unity 3d
		/* legendary-gl */
	];
}
