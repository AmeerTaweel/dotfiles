{
  config,
  pkgs,
  ...
}: {
  # MPD Config
  services.mpd = {
    enable = true;
    musicDirectory = config.xdg.userDirs.music;
    network = {
      startWhenNeeded = true;
    };
    extraConfig = ''
      # process id of mpd
      # pid_file "XXX"

      # mpd state location
      # state_file "XXX"

      # dynamic music information
      # sticker_file "XXX"

      # refresh database when files in music directory change
      auto_update "yes"

      # audio config
      audio_output {
        type "pulse"
        name "pulse"
      }
    '';
  };

  # Enable PlayerCTL
  services.playerctld.enable = true;

  # NCMPCPP MPD Client
  programs.ncmpcpp = {
    enable = true;
    mpdMusicDir = /.${config.xdg.userDirs.music};
    bindings = [
      # Vim movement keys
      {
        key = "h";
        command = "previous_column";
      }
      {
        key = "j";
        command = "scroll_down";
      }
      {
        key = "k";
        command = "scroll_up";
      }
      {
        key = "l";
        command = "next_column";
      }
    ];
    settings = {
      # Use alternative interface
      user_interface = "alternative";
      # Display songs consistently
      playlist_editor_display_mode = "columns";
      search_engine_display_mode = "columns";
      browser_display_mode = "columns";
      playlist_display_mode = "columns";
      song_columns_list_format = "(30)[yellow]{a} (30)[white]{t} (30)[cyan]{b} (10)[red]{l}";
    };
  };

  # Install programs
  home.packages = with pkgs; [
    mpdris2 # control mpd via playerctl
    mpc_cli
    picard # auto music tagger
    za-zombie # music downloader
  ];

  # AutoStart mpdris2
  autostart.mpdris2 = {
    description = "Autostart mpdris2";
    exec = "${pkgs.mpdris2}/bin/mpDris2";
  };
}
