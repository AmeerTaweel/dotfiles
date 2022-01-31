{ ... }:

let
	browserList = [ "google-chrome.desktop" "firefox.desktop" ];
in {
	xdg.mime.enable = true;
	xdg.mimeApps.defaultApplications = {
		"text/html" = browserList;
		"x-scheme-handler/http" = browserList;
		"x-scheme-handler/https" = browserList;
		"x-scheme-handler/about" = browserList;
		"x-scheme-handler/unknown" = browserList;
	};
}
