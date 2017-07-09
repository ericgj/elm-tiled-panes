# elm-tiled-panes

An experiment using elm + CSS Grid to build an in-browser "tiled panes UI" 
(similar to tmux or tiled window manager.)

See [here](/ericgj/elm-tiled-panes/blob/master/example/TileLayout.elm) for
usage, and [live example](https://runelm.io/c/k2x).

Some unsolved problems:

  - How to specify both overall grid height, and min-height of tiles.
  - Custom resizing of tiles
  - Removal of tiles that doesn't result in entire branches being removed,
    or tiles being orphaned.

