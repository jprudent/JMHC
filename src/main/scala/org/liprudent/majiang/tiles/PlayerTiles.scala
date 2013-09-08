package org.liprudent.majiang.tiles

import org.liprudent.majiang.tiles.Types._
import org.liprudent.majiang.figures.{Figure, OrdFigure, Bonus, Kong}

/**
 * Represents the different sets of tiles owned by a player.
 *
 * @param concealed The hand containing the closed tiles of a player
 * @param melded The tiles melded as figures
 * @param concealedKongs List of hidden kongs. By default empty list
 * @param bonus Flowers and seasons. Default is Nil.
 *
 * @note Melded Kongs are included in `melded`
 *       TODO there are too much parameters.
 */
case class PlayerTiles(concealed: ConcealedTiles, melded: Figures, concealedKongs: List[Kong] = Nil,
                       bonus: Bonus = Bonus(Nil)) {

  require(melded.sorted(OrdFigure) == melded, "melded not sorted")

  require(concealed.tileSet.toTiles.contains(concealed.lastTileContext.tile) ||
    concealedKongs.exists(_.tile == concealed.lastTileContext.tile), concealed.lastTileContext.tile + " not in " +
    concealed.tileSet.toTiles + " nor in " + concealedKongs)

  lazy val numberOfTiles = concealed.tileSet.size + disclosedSize + concealedKongsSize

  lazy val disclosedSize = count(melded)

  lazy val concealedKongsSize = count(concealedKongs)

  lazy val numberOfKongs = concealedKongs.size + melded.filter(_.isInstanceOf[Kong]).size

  private def count(figures: List[Figure]) =
    figures.foldLeft(0)((sum: Int, f: Figure) => sum + f.properties.size)
}
