from libqtile.lazy   import lazy
from libqtile.config import EzKey, Group

# +-------------------+
# | Group Definitions |
# +-------------------+

groups = [Group(i) for i in "1234567890"]

# +----------------+
# | Group Bindings |
# +----------------+

switch = (EzKey(
    f"M-{i.name}",
    lazy.group[i.name].toscreen(),
    desc = f"Switch to group {i.name}",
) for i in groups)

move = (EzKey(
    f"M-S-{i.name}",
    lazy.window.togroup(i.name),
    desc = f"Move focused window to group {i.name}",
) for i in groups)

binds = [
    *switch,
    *move,
]
