{ ... }:

let
	browserList = [ "nani.desktop" "google-chrome.desktop" "firefox.desktop" ];
	videoPlayerList = [ "mpv.desktop" "vlc.desktop" ];
in {
	xdg.mime.enable = true;
	xdg.mimeApps.enable = true;
	xdg.mimeApps.defaultApplications = {
		"text/html" = browserList;
		"x-scheme-handler/http" = browserList;
		"x-scheme-handler/https" = browserList;
		"x-scheme-handler/about" = browserList;
		"x-scheme-handler/unknown" = browserList;
		"video/mp4" = videoPlayerList;
	};
}
