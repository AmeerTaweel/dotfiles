local command_widget = require("ui.widgets.shell_command")

return command_widget.setup(
	"date +'%H:%M:%S'",
	{ pre_text = "🕜 " }
)
