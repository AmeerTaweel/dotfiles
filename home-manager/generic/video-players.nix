{pkgs, ...}: {
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

  home.packages = [pkgs.vlc];
}
