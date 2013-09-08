package org.liprudent.majiang

import org.liprudent.majiang.tiles.{Tile, TileSet}
import org.liprudent.majiang.figures.{Kong, Figure}
;

object UniqueWait {

  /**
   *
   * @param concealed tiles before winning
   * @param melded figures
   * @param concealedKongs
   * @param exclude A collection of tiles to exclude from result.
   *                Typically if you already computed a valid mahjong, you know that last tile is
   *                a result and you don't want to recompute that.
   *                Default is `Nil`
   * @return the list of waiting tiles
   */
  def waitingTiles(concealed: TileSet, melded: List[Figure], concealedKongs: List[Kong], exclude: Seq[Tile] = Nil): List[Tile] = {


    def satisfy(tile: Tile): Boolean = {
      val added: TileSet = concealed.added(tile)
      val allCombinations = computer.FindAllAndReduce(added).allFiguresCombinations
      allCombinations.filter(possibleClosed => HuFinder.isWellFormedMahjong(possibleClosed, melded,
        concealedKongs)).size > 0
    }

    //all tiles to try. Filter out tiles that has 4 occurence (there can not be 5)
    val tilesToTry = Tile.allButBonus.filterNot(tile =>
      concealed.occurence(tile) >= 4 || exclude.contains(tile)
    )

    tilesToTry.filter(satisfy).toList.sorted
  }

}
