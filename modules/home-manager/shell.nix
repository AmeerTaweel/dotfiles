{
  config,
  params,
  pkgs,
  ...
}: let 
  flake-dir = "${config.home.homeDirectory}/dotfiles/devices/${params.hostname}";
  nixos-flake = "${flake-dir}#${params.hostname}";
  nix-summary = "${pkgs.nixos-rebuild-summary}/bin/nixos-rebuild-summary";

  shellAliases = {
    # Fast `cd` to parent directory
    ".." = "cd ..";
    ".1" = "cd ..";
    ".2" = "cd ../..";
    ".3" = "cd ../../..";
    ".4" = "cd ../../../..";
    ".5" = "cd ../../../../..";

    # Create parent directories on demand
    mkdir = "mkdir -pv";

    # Most common `nixos-rebuild` operations
    nx-boot = "sudo nixos-rebuild boot --flake ${nixos-flake} && ${nix-summary}";
    nx-build = "nixos-rebuild build --flake ${nixos-flake}";
    nx-switch = "sudo nixos-rebuild switch --flake ${nixos-flake} && ${nix-summary}";
  };
in {
  imports = [
    ./rar.nix
    ./top.nix
  ];

  home.sessionVariables = {
    GNUPGHOME = "${config.xdg.dataHome}/gnupg";
  };

  home.shellAliases = {
    # Enable `grep` colors when the output is a terminal
    grep = "grep --color=auto";
    egrep = "egrep --color=auto";
    fgrep = "fgrep --color=auto";

    # Tolerate Mistakes
    sl = "ls";
    "cd.." = "cd ..";

    # Alias `wget` to use a custom hsts cache file location:
    wget = "wget --hsts-file='${config.xdg.dataHome}/wget-hsts'";
  };

  programs.bash = {
    enable = true;
    initExtra = ''
      # Enable Vi bindings
      set -o vi
    '';
    historyFile = "${config.xdg.stateHome}/bash/history";
    historyIgnore = ["ls" "cd" "exit"];
    inherit shellAliases;
  };

  programs.nushell = {
    enable = true;
    extraConfig = ''
      $env.config.show_banner = false
      
      def prompt_path_gadget [] {
        let path = (pwd | str replace -r $"^($env.HOME)" "~")
        return $path
      }
      
      def prompt_level_gadget [] {
        let shell_level = $env.SHLVL
        if $shell_level > 1 {
          return $"(ansi blue)[shlvl: ($shell_level)](ansi reset)"
        } else {
          return ""
        }
      }
      
      def prompt_time_gadget [] {
        let last_time = $env.CMD_DURATION_MS | into int | into duration --unit ms
        return $"(ansi blue)[ ($last_time)](ansi reset)"
      }
      
      def prompt_exit_gadget [] {
        let last_exit = $env.LAST_EXIT_CODE
        if $last_exit != 0 {
          return $"(ansi red)[exit: ($last_exit)](ansi reset)"
        } else {
          return ""
        }
      }
      
      def prompt_git_branch_gadget [] {
        use std
        try {
          let branch = (git branch --show-current e> (std null-device))
          return $"(ansi yellow)[󰘬 ($branch)](ansi reset)"
        } catch {
          return ""
        }
      }
      
      $env.PROMPT_COMMAND = { ||
      
        let gadgets = [
          (prompt_path_gadget)
      	(prompt_level_gadget)
      	(prompt_time_gadget)
      	(prompt_git_branch_gadget)
      	(prompt_exit_gadget)
        ]
      
        $gadgets | where $it != "" | str join " "
      }
      $env.PROMPT_INDICATOR_VI_NORMAL = " [N]\n> "
      $env.PROMPT_INDICATOR_VI_INSERT = " [I]\n> "
      $env.PROMPT_COMMAND_RIGHT = ""
      $env.config.edit_mode = "vi"

      def nx-boot [] {
        sudo nixos-rebuild boot --flake ${nixos-flake}
        ${nix-summary}
      }
      def nx-build [] {
        nixos-rebuild build --flake ${nixos-flake}
      }
      def nx-switch [] {
        sudo nixos-rebuild switch --flake ${nixos-flake}
        ${nix-summary}
      }

      $env.config.hooks.command_not_found = { |cmd_name|
        let install = { |pkgs|
          $pkgs | each {|pkg| $"  nix shell nixpkgs#($pkg)" }
        }
        let run_once = { |pkgs|
          $pkgs | each {|pkg| $"  nix shell nixpkgs#($pkg) --command '($cmd_name) ...'" }
        }
        let single_pkg = { |pkg|
          let lines = [
            $"The program '($cmd_name)' is currently not installed."
            ""
            "You can install it by typing:"
            (do $install [$pkg] | get 0)
            ""
            "Or run it once with:"
            (do $run_once [$pkg] | get 0)
          ]
          $lines | str join "\n"
        }
        let multiple_pkgs = { |pkgs|
          let lines = [
            $"The program '($cmd_name)' is currently not installed. It is provided by several packages."
            ""
            "You can install it by typing one of the following:"
            (do $install $pkgs | str join "\n")
            ""
            "Or run it once with:"
            (do $run_once $pkgs | str join "\n")
          ]
          $lines | str join "\n"
        }
        let pkgs = (nix-locate --minimal --no-group --type x --type s --top-level --whole-name --at-root $"/bin/($cmd_name)" | lines)
        let len = ($pkgs | length)
        let ret = match $len {
          0 => null,
          1 => (do $single_pkg ($pkgs | get 0)),
          _ => (do $multiple_pkgs $pkgs),
        }
        return $ret
      }
    '';
    environmentVariables = config.home.sessionVariables;
  };

  programs.fish = {
    enable = true;
    interactiveShellInit = ''
      ${builtins.readFile ./fish_config.fish}
      ${pkgs.nix-your-shell}/bin/nix-your-shell fish | source
    '';
    inherit shellAliases;
  };

  # Easy directory jumping in all shells
  programs.zoxide = {
    enable = true;
    enableBashIntegration = true;
    enableFishIntegration = true;
    enableNushellIntegration = true;
    enableZshIntegration = true;
  };

  programs.eza.enable = true;

  programs.direnv = {
    enable = true;
    nix-direnv = {
      enable = true;
    };
  };

  home.packages = with pkgs; [
    cht-sh # Access cheatsheets from terminal

    file # Determina file type
    tree # List directory contents in a tree-like format
    curl # Transfer URLs

    ffmpeg # Video Converter

    ripgrep # `grep` clone
    fd # `find` clone
    bat # `cat` clone with syntax highlighting and `git` integration
    sd # `sed` replacement

    zip # Zip Compression
    unzip # Zip Decompression

    entr # Run arbitrary commands when files change
    watch # Execute a command repeatedly, and monitor the output in full-screen mode

    # clipboard-jh # Terminal clipboard

    eva # Calculator

    sshfs

    mosh
  ];

  # `tldr` comand
  programs.tealdeer = {
    enable = true;
    settings.updates.auto_update = true;
  };
}
