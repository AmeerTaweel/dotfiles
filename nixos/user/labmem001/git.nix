{ user, pkgs, ... }:

let
in {
	programs.git = {
		enable = true;
		userName = user.fullName;
		userEmail = user.email;
		extraConfig = {
			init = {
				defaultBranch = "master";
			};
		};
	};
}
