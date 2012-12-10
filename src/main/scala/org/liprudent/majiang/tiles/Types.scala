package org.liprudent.majiang.tiles

import org.liprudent.majiang.figures.Figure

object Types {
  type Occurence = Int
  type TileOccurence = (Tile, Occurence)
  type Figures = List[Figure]
}
