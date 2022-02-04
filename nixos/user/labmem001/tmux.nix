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

	customTmuxPackages.tmux-fzf = pkgs.tmuxPlugins.mkTmuxPlugin {
		pluginName = "tmux-fzf";
		rtpFilePath = "main.tmux";
		version = "unstable-2020-12-07";
		src = pkgs.fetchFromGitHub {
			owner = "sainnhe";
			repo = "tmux-fzf";
			rev = "1801dd525b39154745ea668fb6916035023949e3";
			sha256 = "e929Jqletmobp3WAR1tPU3pJuYTYVynxc5CvB80gig8=";
		};
		postInstall = ''
			find $target -type f -print0 | xargs -0 sed -i -e 's|fzf |${pkgs.fzf}/bin/fzf |g'
			find $target -type f -print0 | xargs -0 sed -i -e 's|sed |${pkgs.gnused}/bin/sed |g'
			find $target -type f -print0 | xargs -0 sed -i -e 's|tput |${pkgs.ncurses}/bin/tput |g'
		'';
	};
in {
    programs.tmux = {
        enable = true;
		plugins = with pkgs.tmuxPlugins; [{
            plugin = customTmuxPackages.vim-tmux-navigator;
		} {
			plugin = customTmuxPackages.tmux-fzf;
			extraConfig = ''
				TMUX_FZF_SESSION_FORMAT="#{session_windows} windows"
				TMUX_FZF_OPTIONS="-p -w 62% -h 38% --cycle --bind 'tab:toggle-down,btab:toggle-up'"
				bind-key "s" run-shell -b "${customTmuxPackages.tmux-fzf}/share/tmux-plugins/tmux-fzf/scripts/session.sh attach"
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
