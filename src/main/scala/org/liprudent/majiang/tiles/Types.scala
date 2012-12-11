package org.liprudent.majiang.tiles

import org.liprudent.majiang.figures.Figure

object Types {
  type Occurence = Int
  type TileOccurence = (Tile, Occurence)
  type Figures = List[Figure]

  object Figures {
    implicit def size(figures: Figures): Int =
      figures.foldLeft(0)((total, fig) => total + fig.properties.size)
  }

}
