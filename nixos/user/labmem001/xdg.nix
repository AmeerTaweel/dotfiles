{ user, pkgs, config, ... }:

let
	browserList = [ "google-chrome.desktop" "firefox.desktop" ];
	videoPlayerList = [ "mpv.desktop" "vlc.desktop" ];
	homeDir = config.home.homeDirectory;
in {
	xdg.enable = true;

	xdg.userDirs = {
		enable = true;
		createDirectories = true;
		desktop = "${homeDir}";
		documents = "${homeDir}";
		download = "${homeDir}/downloads";
		music = "${homeDir}/music";
		pictures = "${homeDir}";
		publicShare = "${homeDir}";
		templates = "${homeDir}";
		videos = "${homeDir}";
	};

	xdg.desktopEntries = {
		zathura = {
			type = "Application";
			name = "Zathura";
			genericName = "Document Viewer";
			exec = "${pkgs.zathura}/bin/zathura %U";
			categories = [ "Application" ];
			mimeType = [ "application/pdf" ];
		};
		mpv = {
			type = "Application";
			name = "mpv";
			genericName = "Video Player";
			exec = "${pkgs.mpv}/bin/mpv %U";
			categories = [ "Application" ];
			mimeType = [ "video/mp4" ];
		};
	};

	xdg.mime.enable = true;
	xdg.mimeApps.enable = true;
	xdg.mimeApps.defaultApplications = {
		"text/html" = browserList;
		"x-scheme-handler/http" = browserList;
		"x-scheme-handler/https" = browserList;
		"x-scheme-handler/about" = browserList;
		"x-scheme-handler/unknown" = browserList;
		"video/mp4" = videoPlayerList;
		"application/pdf" = "zathura.desktop";
	};
}
