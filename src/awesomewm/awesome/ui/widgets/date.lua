local command_widget = require("ui.widgets.shell_command")

return command_widget.setup(
	"date +'%a, %d %b'",
	{ pre_text = "📆 " }
)
