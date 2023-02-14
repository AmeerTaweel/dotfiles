{pkgs, ...}: {
  # MPV
  programs.mpv = {
    enable = true;
    defaultProfiles = ["gpu-hq"];
    config = {
      no-keepaspect-window = "";
      cache = "yes";
      video-sync = "display-resample";
      vo = "gpu";
      hwdec = "auto";
      sub-font = "Noto Sans Arabic";
    };
    scripts = with pkgs.mpvScripts; [mpris];
  };

  xdg.desktopEntries.mpv = {
    type = "Application";
    name = "mpv";
    genericName = "Video Player";
    exec = "${pkgs.mpv}/bin/mpv %U";
    categories = ["Application"];
    mimeType = ["video/mp4"];
  };

  home.packages = [pkgs.vlc];
}
