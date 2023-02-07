from libqtile.lazy   import lazy
from libqtile.config import EzClick, EzDrag

# +----------------+
# | Mouse Bindings |
# +----------------+

binds = [
    EzDrag ("M-<Button1>", lazy.window.set_position_floating(), start = lazy.window.get_position()),
    EzDrag ("M-<Button3>", lazy.window.set_size_floating(),     start = lazy.window.get_size()),
    EzClick("M-<Button2>", lazy.window.bring_to_front()),
]
