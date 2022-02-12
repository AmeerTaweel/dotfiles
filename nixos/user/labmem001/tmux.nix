{ pkgs, ... }:

let
    tmuxRCPath = ../../../tmux/rc.tmux;
    tmuxConfigPath = ../../../tmux/config;

	customTmuxPackages.vim-tmux-navigator = pkgs.tmuxPlugins.mkTmuxPlugin {
		pluginName = "vim-tmux-navigator";
		rtpFilePath = "vim-tmux-navigator.tmux";
		version = "unstable-2019-12-10";
		src = pkgs.fetchFromGitHub {
			owner = "AmeerTaweel";
			repo = "vim-tmux-navigator";
			rev = "82b5e1242f5a2c3a3e68b891c6eb0c33e155eb9f";
			sha256 = "htnnjhFUNqoLszSVkpeAOsujtScCQ3sp9XDAOydG2XQ=";
		};
	};
in {
    programs.tmux = {
        enable = true;
		plugins = with pkgs.tmuxPlugins; [{
            plugin = customTmuxPackages.vim-tmux-navigator;
		} {
			plugin = tmux-fzf;
			extraConfig = ''
				TMUX_FZF_SESSION_FORMAT="#{session_windows} windows"
				TMUX_FZF_OPTIONS="-p -w 62% -h 38% --cycle --bind 'tab:toggle-down,btab:toggle-up'"
				bind-key "s" run-shell -b "${tmux-fzf}/share/tmux-plugins/tmux-fzf/scripts/session.sh attach"
			'';
		}];
    };

	home.file.tmuxRC = {
		source = tmuxRCPath;
		target = ".tmux.conf";
	};
    
	xdg.configFile.tmuxConfig = {
		source = tmuxConfigPath;
		target = "tmux/config";
		recursive = true;
	};
}
