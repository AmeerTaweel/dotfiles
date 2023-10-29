{ ... }: {
  # Set GNOME GSettings
  dconf.settings = {
    "org/gnome/desktop/interface" = {
      # Use dark theme
      color-scheme = "prefer-dark";
      # Disable hot corner
      # Touch the top-left corner to open the Activities Overview
      enable-hot-corners = false;
      # Top Bar
      clock-show-weekday = true;
      show-battery-percentage = false;
    };

    "org/gnome/mutter" = {
      # Active Screen Edges
      # Drag windows against screen edges to resize them
      edge-tiling = true;
      dynamic-workspaces = true;
      center-new-windows = true;
    };

    "org/gnome/desktop/notifications" = {
      # Disable lock screen notifications
      show-in-lock-screen = false;
    };

    "org/gnome/desktop/input-sources" = {
      # Use capslock as ctrl
      xkb-options = ["caps:ctrl_modifier"];
    };

    "org/gnome/desktop/peripherals/keyboard" = {
      # Enable numlock by default
      numlock-state = true;
    };

    "org/gnome/desktop/peripherals/touchpad" = {
      click-method = "areas";
      natural-scroll = false;
      tap-to-click = true;
      two-finger-scrolling-enabled = true;
    };

    "org/gnome/desktop/privacy" = {
      remember-recent-files = true;
      # Remember file history for the last 30 days
      recent-files-max-age = 30;

      remove-old-temp-files = true;
      remove-old-trash-files = true;
      # Auto delete after 30 days
      old-files-age = "uint32 30";
    };

    "org/gnome/desktop/screensaver" = {
      # Auto lock immediately
      lock-delay = "uint32 0";
      # Auto screen lock
      lock-enabled = true;
    };

    "org/gnome/desktop/session" = {
      # Disable blank screen on inactivity
      idle-delay = "uint32 0";
    };

    "org/gnome/desktop/sound" = {
      allow-volume-above-100-percent = true;
    };

    "org/gnome/settings-daemon/plugins/power" = {
      # Dim screen on inactivity
      idle-dim = false;

      # Disable auto suspend when charging
      sleep-inactive-ac-type = "nothing";

      # Auto suspend after 30 minutes of inactivity if on battery power
      sleep-inactive-battery-type = "suspend";
      sleep-inactive-battery-timeout = 1800;

      # Auto power saver on low battery
      power-saver-profile-on-low-battery = true;
    };

    "org/gnome/shell" = {
      disable-user-extensions = false;
      enabled-extensions = [
        "appindicatorsupport@rgcjonas/gmail/com"
        "drive-menu@gnome-shell-extensions/gcampax/github/com"
        "pano@elhan/io"
        "hibernate-status@dromi"
        "just-perfection-desktop@just-perfection"
      ];
    };

    "org/gnome/shell/app-switcher" = {
      # Include apps from current workspace when switching apps
      current-workspace-only = true;
    };

    "org/gnome/shell/extensions/just-perfection" = {
      activities-button = false;
      events-button = false;
      world-clock = false;
    };

    # Pano Clipboard Manager
    "org/gnome/shell/extensions/pano" = {
      history-length = 500;
      keep-search-entry = false;
      play-audio-on-copy = false;
      send-notification-on-copy = false;
    };
  };
}
