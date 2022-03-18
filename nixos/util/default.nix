{ nixpkgs, nur, home-manager, ... }:
with builtins;
{
	mkUser = user@{
		username,
		fullName,
		email,
		editor,
		terminal,
		theme,
		system,
		shell,
		...
	}:
	let
		pkgs = import nixpkgs { inherit system; }; 
	in {
		inherit username fullName;
		systemModule = { ... }: {
			users.users.${username} = {
				createHome = true;
				description = fullName;
				extraGroups = [
					"wheel" # Enable "sudo" for the user.
					"networkmanager"
					"video"
					"libvirtd"
				];
				# Don't forget to change the password with "passwd".
				initialPassword = "";
				isNormalUser = true;
				shell = pkgs.${shell};
			};
			programs.${shell}.enable = true;
		};
		homeManagerModule = { host, ... }: {
			name = username;
			value = import ../user/${username} { inherit host user; };
		};
	};

	# NIC: Network Interface Card
	# nics: List of host NIC names.
	mkHost = host@{
		hostName,
		system,
		users,
		nics,
		timezone,
		latitude,
		longitude,
		...
	}:
	nixpkgs.lib.nixosSystem {
		inherit system;
		modules = [
			{ _module.args = { inherit host; }; }
			{ nixpkgs.overlays = [ nur.overlay ]; }
			../host/common
			../host/${hostName}
			home-manager.nixosModules.home-manager
			({ pkgs, config, ... }: {
				home-manager.useGlobalPkgs = true;
				home-manager.useUserPackages = true;
				home-manager.users = listToAttrs (map (user: (user.homeManagerModule { inherit host; })) users);
			})
		] ++ (map (user: user.systemModule ) users);
	};
}
