{config, pkgs, ...}: let
  shellAliases = {
    # Enable `grep` colors when the output is a terminal
    grep = "grep --color=auto";
    egrep = "egrep --color=auto";
    fgrep = "fgrep --color=auto";

    # Fast `cd` to parent directory
    ".." = "cd ..";
    ".1" = "cd ..";
    ".2" = "cd ../..";
    ".3" = "cd ../../..";
    ".4" = "cd ../../../..";
    ".5" = "cd ../../../../..";

    # Create parent directories on demand
    mkdir = "mkdir -pv";

    # Tolerate Mistakes
    sl = "ls";
    "cd.." = "cd ..";
  };
in {
  imports = [
    ./rar.nix
    ./top.nix
  ];

  home.sessionVariables = {
    HISTFILE = "${config.xdg.stateHome}/bash/history";
  };

  programs.bash = {
    enable = true;
    initExtra = ''
      # Enable Vi bindings
      set -o vi
    '';
    historyIgnore = ["ls" "cd" "exit"];
    inherit shellAliases;
  };

  programs.fish = {
    enable = true;
    shellInit = ''
      # Enable Vi bindings
      fish_vi_key_bindings
      # Turn off the greeting message
      set fish_greeting
    '';
    inherit shellAliases;
  };

  # Easy directory jumping in all shells
  programs.autojump.enable = true;

  programs.exa = {
    enable = true;
    enableAliases = true;
  };

  programs.direnv = {
    enable = true;
    nix-direnv = {
      enable = true;
    };
  };

  home.packages = with pkgs; [
    cht-sh
    file
    tree
    ripgrep
    fd
    curl
    bat
    ffmpeg

    zip
    unzip

    # Run arbitrary commands when files change
    entr
    # Execute a command repeatedly, and monitor the output in full-screen mode
    watch
  ];

  # `tldr` comand
  programs.tealdeer = {
    enable = true;
    settings.updates.auto_update = true;
  };
}
