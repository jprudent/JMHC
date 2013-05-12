package org.liprudent.majiang.tiles

import scala.Predef._


case class BitTileSet(bamboos: BitTileSet.BitFamily) extends TileSet {

  import BitTileSet._

  val size: Int = sizeOf(bamboos);

  def removed(tile: Tile): BitTileSet = {
    require(tile.family == Bamboo, "only implemented on bamboos")
    val newFamily = changeOccurrence(bamboos, tile, (occ:Int) => occ - 1, (occ:Int) => occ > 0)
    this.copy(bamboos = newFamily)
  }

  def added(tile: Tile) = {
    require(tile.family == Bamboo, "only implemented on bamboos")
    val newFamily = changeOccurrence(bamboos, tile, (occ:Int) => occ + 1, (occ:Int) => occ < 4)
    this.copy(bamboos = newFamily)
  }

  def isAllUnique =  (oneOfEach | bamboos) == oneOfEach

  private def changeOccurrence(family: BitFamily, tile:Tile, occModificator: (Int) => Int, requireCheck: (Int) => Boolean) = {

    val tileOccurence = occurenceOf(bamboos, tile.value)
    require(requireCheck(tileOccurence))

    val newTileOccurence = occModificator(tileOccurence)

    // move the new occurence to original place. Like 0000X0000 (base 8)
    val positionedTile = newTileOccurence << ((9 - tile.value) * 3)

    // zeroes tiles in original family: 111102222
    val zeroed = family & inversedMaskForValue(tile.value - 1)

    // finally mix the zeroed with the original
    zeroed | positionedTile
  }

  /**
   *
   * @param family
   * @return number of tiles in `family`
   */
  private def sizeOf(family: Int) = {
    (1 to 9).fold(0) {
      (somme: Int, i: Int) => somme + occurenceOf(family, i)
    }
  }

  /**
   *
   * @param family
   * @param value
   * @return The number of occurences for the tile with `value` in `family`
   */
  private def occurenceOf(family: Int, value: Tile.TileValue) = {
    // zeroes everything except tile
    val onlyTile = family & maskForValue(value - 1)
    // then shift it at the end, so it's like 00000000X (base 4)
    onlyTile >>> ((9 - value) * 3)
  }


}

object BitTileSet {

  type BitFamily = Int

  val maskForValue = Array(
    Integer.parseInt("700000000", 8),
    Integer.parseInt("070000000", 8),
    Integer.parseInt("007000000", 8),
    Integer.parseInt("000700000", 8),
    Integer.parseInt("000070000", 8),
    Integer.parseInt("000007000", 8),
    Integer.parseInt("000000700", 8),
    Integer.parseInt("000000070", 8),
    Integer.parseInt("000000007", 8))

  val inversedMaskForValue = maskForValue.map(_ ^ Integer.parseInt("777777777", 8))

  val oneOfEach = Integer.parseInt("111111111",8)

}
