{ pkgs, ... }:

{
	# Install
	home.packages = [ pkgs.copyq ];

	# AutoStart
	systemd.user.services.autostart-copyq = {
		Install = {
			WantedBy = [ "graphical-session.target" ];
		};

		Service = {       
			ExecStartPre="${pkgs.coreutils-full}/bin/sleep 3";
			ExecStart = "${pkgs.copyq}/bin/copyq";
			Restart = "on-failure";
			RestartSec = 3;
		};

		Unit = {
			After = [ "graphical-session-pre.target" "tray.target" ];
			Description = "Autostart CopyQ clipboard manager";
			PartOf = [ "graphical-session.target" ];
			Requires = [ "tray.target" ];
		};
	};
	
	# Configure
	xdg.configFile.copyqConfig = {
		source = ../../../copyq;
		target = "copyq";
		recursive = true;
	};
}
