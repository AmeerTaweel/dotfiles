{
	description = "NixOS Configuration";

	inputs = {
		# Nixpkgs
		nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

		# Home Directory Configuration
		home-manager.url = "github:nix-community/home-manager";

		# Nix User Repository
		nur.url = "github:nix-community/NUR";

		# Music Downloader
		za-zombie.url = "github:AmeerTaweel/za-zombie";

		# VSCode Extensions
		nix-vscode-marketplace.url = "github:AmeerTaweel/nix-vscode-marketplace";

		# Fish shell plugin for directory jumping
		fish-z.url = "github:jethrokuan/z";
		fish-z.flake = false;
	};

	outputs = inputs@{ self, nixpkgs, home-manager, ... }: 
	let
		util = import ./nixos/util {
			inherit nixpkgs home-manager;
			flake-inputs = inputs;
		};

		users = {
			labmem001 = util.mkUser {
				username = "labmem001";
				fullName = "Ameer Taweel";
				groups = [
					"wheel" # Enable "sudo" for the user.
					"networkmanager"
					"video"
					"libvirtd"
					"docker"
				];
				email = "ameertaweel2002@gmail.com";
				editor = "nvim";
				terminal = "kitty";
				theme = "ayu-dark";
				shell = "fish";
			};
		};
	in {
		nixosConfigurations = with users; {
			fg001 = util.mkHost rec {
				hostName = "fg001";
				system = "x86_64-linux";
				stateVersion = "21.11";
				users = [ labmem001 ];
				nics = [ "eno1" "wlo1" ];
				timezone = "Europe/Istanbul";
				latitude = 41.20504275770959;
				longitude = 29.070748249158278;
				overlays = with inputs; [
					nur.overlay
					za-zombie.overlays.${system}.default
					nix-vscode-marketplace.overlays.${system}.default
				];
			};
		};
	};
}
