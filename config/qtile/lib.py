from libqtile.config import EzConfig, Key, KeyChord

class EzKeyChord(EzConfig, KeyChord):
    """
    Define a key chord using the Emacs-like format.

    Parameters
    ==========
    keydef:
        The Emacs-like key specification, e.g. ``"M-S-a"``.
    submappings:
        A list of :class:`Key` or :class:`KeyChord` declarations to bind in this chord.
    mode:
        Boolean. Setting to ``True`` will result in the chord persisting until
        Escape is pressed. Setting to ``False`` (default) will exit the chord once
        the sequence has ended.
    name:
        A string to name the chord. The name will be displayed in the Chord
        widget.
    desc:
        A string to describe the chord. This attribute is not directly used by Qtile
        but users may want to access this when creating scripts to show configured
        keybindings.

    """

    def __init__(
        self,
        keydef: str,
        submappings: list[Key | KeyChord],
        mode: bool | str = False,
        name: str = "",
        desc: str = "",
    ):
        modkeys, key = self.parse(keydef)
        super().__init__(modkeys, key, submappings, mode, name)
