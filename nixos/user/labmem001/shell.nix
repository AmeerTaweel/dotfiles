{ pkgs, ... }:

{
    programs.bash = {
        enable = true;
		initExtra = ''
			# Enable Vi bindings
			set -o vi
		'';
		historyIgnore = [ "ls" "cd" "exit" ];
		shellAliases = {
			# grep aliases
			grep = "grep --color=auto";

			# Open Tmux with UTF8 support
			tmux = "tmux -u";
		};
    };

	programs.exa = {
		enable = true;
		enableAliases = true;
	};
}
