import group
import key
import layout
import mouse
import scratch
import screen

widget_defaults = dict(
    font     = "Hack Nerd Font",
    fontsize = 16,
    padding  = 3,
)
extension_defaults = widget_defaults.copy()

screens = [
    *screen.screens,
]

layouts = [
    *layout.layouts,
]
floating_layout = layout.floating

groups = [
    *group.groups,
    *scratch.groups,
]

keys = [
    *key.binds,
    *group.binds,
    *scratch.binds,
]

mouse = [
    *mouse.binds,
]

dgroups_key_binder         = None
dgroups_app_rules          = []
follow_mouse_focus         = True
bring_front_click          = False
cursor_warp                = False
auto_fullscreen            = True
focus_on_window_activation = "smart"
reconfigure_screens        = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = False

# Nobody really uses or cares about this string besides java UI toolkits.
# You can see several discussions on the mailing lists, GitHub issues, and
# other WM documentation that suggest setting this string if your java app
# doesn't work correctly. We may as well just lie and say that we're a working
# one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
