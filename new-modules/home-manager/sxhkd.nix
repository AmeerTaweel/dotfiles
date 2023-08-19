{
  config,
  pkgs,
  ...
}: let
  mod = "super";
in {
  services.sxhkd = {
    enable = true;
    # NOTE: Reload SXHKD With: `pkill -usr1 -x sxhkd`
    keybindings = with pkgs; {
      "${mod} + r; p" = "rofi -show drun -display-drun 'launch'";
      "${mod} + r; c" = "rofi -show run";
      "${mod} + r; e" = "rofi -show emoji -matching normal";
      "${mod} + r; s" = "rofi -show ssh -no-parse-known-hosts -disable-history";
      # "super + control + p" = "power-menu";
      # Show all windows
      "${mod} + r; w" = "rofi -show window";
      # Show windows in the current workspace
      "${mod} + r; W" = "rofi -show windowcd -display-windowcd 'window'";
      "{F2,F3}" = "${brightnessctl}/bin/brightnessctl set {10%-,10%+}";
      "${mod} + y" = "copyq show";
      "${mod} + x ; r" = "${maim}/bin/maim --select | ${xclip}/bin/xclip -selection clipboard -target image/png";
      "${mod} + x ; w" = "${xdotool}/bin/xdotool getactivewindow | ${xe}/bin/xe ${maim}/bin/maim --window | ${xclip}/bin/xclip -selection clipboard -target image/png";
      "${mod} + x ; f" = "${maim}/bin/maim | ${xclip}/bin/xclip -selection clipboard -target image/png";
      # Launch a new terminal
      "${mod} + Return" = config.home.sessionVariables.TERMINAL;
      # Audio Keys
      "XF86Audio{Lower,Raise}Volume" = "${pamixer}/bin/pamixer --allow-boost {--decrease,--increase} 5";
      "XF86AudioMute" = "${pamixer}/bin/pamixer --allow-boost --toggle-mute";
      "XF86Audio{Prev,Play,Next}" = "${playerctl}/bin/playerctl --player playerctld {previous,play-pause,next}";
    };
  };
}
