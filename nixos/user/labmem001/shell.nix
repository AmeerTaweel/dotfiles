{ pkgs, ... }:

let
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
		historyIgnore = [ "ls" "cd" "exit" ];
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
		plugins = [{
			name = "z";
			src = pkgs.fetchFromGitHub {
				owner = "jethrokuan";
				repo = "z";
				rev = "45a9ff6d0932b0e9835cbeb60b9794ba706eef10";
				sha256 = "pWkEhjbcxXduyKz1mAFo90IuQdX7R8bLCQgb0R+hXs4=";
			};
		}];
	};

	programs.exa = {
		enable = true;
		enableAliases = true;
	};
}
