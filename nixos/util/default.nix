{ nixpkgs, home-manager, flake-inputs, ... }:
with builtins;
{
	mkUser = user@{
		username,
		fullName,
		groups,
		email,
		editor,
		terminal,
		theme,
		shell,
		...
	}: system:
	let
		pkgs = import nixpkgs { inherit system; }; 
	in {
		inherit username fullName;
		systemModule = { ... }: {
			users.users.${username} = {
				createHome = true;
				description = fullName;
				extraGroups = groups;
				# Don't forget to change the password with "passwd".
				initialPassword = "";
				isNormalUser = true;
				shell = pkgs.${shell};
			};
		};
		homeManagerModule = { host, ... }: {
			name = username;
			value = import ../user/${username} { inherit host user flake-inputs; } // { home.stateVersion = host.stateVersion; };
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
		overlays,
		...
	}:
	let
		# Pass the system attribute to users
		users = map (user: user system) host.users;
	in nixpkgs.lib.nixosSystem {
		inherit system;
		modules = [
			{ 
				_module.args = { inherit host flake-inputs; };
				nixpkgs.overlays = overlays;
			}
			../host/specific/${hostName}
			home-manager.nixosModules.home-manager
			({ pkgs, config, ... }: {
				home-manager.useGlobalPkgs = true;
				home-manager.useUserPackages = true;
				home-manager.users = listToAttrs (map (user: (user.homeManagerModule { inherit host; })) users);
			})
		] ++ (map (user: user.systemModule) users);
	};
}
