{
	# TODO: Use GRUB instead of systemd-boot [ Wiki ]
	# TODO: Enable system fonts
	# TODO: Enable connecting to external monitors [ Wiki ]
	# TODO: Verify accelerated video playback [ Wiki ]
	# TODO: Setup printers and scanners [ Wiki ]
	# TODO: Bootloader + Dual-Boot with Windows [ Wiki ]
	# TODO: Offload Steam games with Nvidia GPU [ Wiki ]
	# TODO: Wine support [ Wiki ]
	# TODO: Configure RedShift [ HomeManager ]
	# TODO: Configure notification service [ HomeManager ]
	# TODO: Configure password-store and password-store sync [ HomeManager ]
	# TODO: Configure screen lock [ HomeManager ]
	# TODO: Configure hotkey daemon [ HomeManager ]
	# TODO: Convert to Wayland
	# TODO: Convert to PipeWire [ Wiki ]

	# NOTE: Sample NixOS Config: https://github.com/imMaturana/nixos-config
	# NOTE: Home-Manager Tutorial: https://youtu.be/CDzgNxoAlnA
	description = "NixOS Configuration";

	inputs = {
		nixpkgs.url = "github:NixOS/nixpkgs";
		nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
		home-manager.url = "github:nix-community/home-manager";
	};

	outputs = { self, nixpkgs, nixpkgs-unstable, home-manager, ... }@inputs: 
	let
		util = import ./nixos/util { inherit nixpkgs home-manager; };

		users = {
			labmem001 = util.mkUser {
				username = "labmem001";
				fullName = "Ameer Taweel";
				email = "ameertaweel2002@gmail.com";
				editor = "neovim";
				theme = "ayu-dark";
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
			};
		};
	};
}
