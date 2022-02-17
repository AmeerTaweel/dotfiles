{
	description = "NixOS Configuration";

	inputs = {
		# nixpkgs.url = "github:NixOS/nixpkgs";
		nixpkgs.url = "github:AmeerTaweel/nixpkgs/pkgs-asyncrun-vim";
		nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
		home-manager.url = "github:nix-community/home-manager";
	};

	outputs = inputs@{ self, nixpkgs, nixpkgs-unstable, home-manager, ... }: 
	let
		util = import ./nixos/util { inherit nixpkgs home-manager; };

		users = {
			labmem001 = util.mkUser {
				username = "labmem001";
				fullName = "Ameer Taweel";
				email = "ameertaweel2002@gmail.com";
				editor = "nvim";
				terminal = "alacritty";
				theme = "ayu-dark";
				system = "x86_64-linux";
				shell = "fish";
			};
		};
	in {
		nixosConfigurations = {
			fg001 = util.mkHost {
				hostName = "fg001";
				system = "x86_64-linux";
				users = [ users.labmem001 ];
				nics = [ "eno1" "wlo1" ];
				timezone = "Europe/Istanbul";
				latitude = 41.20504275770959;
				longitude = 29.070748249158278;
			};
		};
	};
}
