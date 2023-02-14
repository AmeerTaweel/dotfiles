{inputs, ...}: let
  shellAliases = {
    # grep aliases
    grep = "grep --color=auto";

    # Open Tmux with UTF8 support
    tmux = "tmux -u";
  };
in {
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

  programs.nushell = {
    enable = true;
  };

  # Easy directory jumping in all shells
  programs.autojump.enable = true;

  programs.exa = {
    enable = true;
    enableAliases = true;
  };
}
