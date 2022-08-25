{
	description = "NixOS Configuration";

	inputs = {
		nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
		# Nix User Repository
		nur.url = "github:nix-community/NUR";
		za-zombie.url = "github:AmeerTaweel/za-zombie";
		home-manager.url = "github:nix-community/home-manager";
		nix-vscode-marketplace.url = "github:AmeerTaweel/nix-vscode-marketplace/feat-auto-generate";
	};

	outputs = inputs@{ self, nixpkgs, nur, home-manager, ... }: 
	let
		other = with inputs; { inherit za-zombie nix-vscode-marketplace; };
		util = import ./nixos/util { inherit nixpkgs nur home-manager other; };

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
