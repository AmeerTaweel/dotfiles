{
	description = "NixOS Configuration";

	inputs = {
		nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
		# Nix User Repository
		nur.url = "github:nix-community/NUR";
		home-manager.url = "github:nix-community/home-manager";
	};

	outputs = inputs@{ self, nixpkgs, nur, home-manager, ... }: 
	let
		util = import ./nixos/util { inherit nixpkgs nur home-manager; };

		users = {
			labmem001 = util.mkUser {
				username = "labmem001";
				fullName = "Ameer Taweel";
				email = "ameertaweel2002@gmail.com";
				editor = "nvim";
				terminal = "kitty";
				theme = "ayu-dark";
				system = "x86_64-linux";
				shell = "fish";
				xdgDirs = {
					desktop = "$HOME";
					documents = "$HOME";
					download = "$HOME/downloads";
					music = "$HOME/music";
					pictures = "$HOME";
					publicShare = "$HOME";
					templates = "$HOME";
					videos = "$HOME";
				};
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
