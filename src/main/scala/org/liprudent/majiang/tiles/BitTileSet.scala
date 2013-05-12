package org.liprudent.majiang.tiles

import scala.Predef._

/**
 *
 * @param bamboos defaults to none
 * @param characters defaults to none
 * @param stones defaults to none
 * @param honors defaults to none
 */
case class BitTileSet(bamboos: BitTileSet.BitFamily = 0, characters: BitTileSet.BitFamily = 0, stones: BitTileSet.BitFamily = 0, honors: BitTileSet.BitFamily = 0) extends TileSet {

  import BitTileSet._

  private type Occurrence = Int

  val size = sizeOf(bamboos)

  def removed(tile: Tile): BitTileSet = {
    changeOccurrence(tile, _ - 1, _ > 0)
  }

  def added(tile: Tile) = {
    changeOccurrence(tile, _ + 1, _ < 4)
  }

  def isAllUnique = (oneOfEach | bamboos | characters | stones | honors) == oneOfEach

  /**
   * Change the number of occurrence of a tile
   * @param tile The tile to change the number of occurrence of
   * @param occModificator a function that take a number of occurrence and return a new number of occurrence
   * @param requireCheck a function that takes the current number of occurrence, used as a validator before changing occurrence. 
   * @return A new `BitTileSet` with occurrence of `tile` changed
   */
  private def changeOccurrence(tile: Tile, occModificator: (Occurrence) => Occurrence, requireCheck: (Occurrence) => Boolean) = {

    val family: BitFamily = selectFamily(tile.family)

    val tileOccurence = occurrenceOf(family, tile.value)
    require(requireCheck(tileOccurence))

    val newTileOccurence = occModificator(tileOccurence)

    // move the new occurrence to original place. Like 0000X0000 (base 8)
    val positionedTile = newTileOccurence << ((9 - tile.value) * 3)

    // zeroes tiles in original family: 111102222
    val zeroed = family & inversedMaskForValue(tile.value - 1)

    // finally mix the zeroed with the original
    val newFamily = zeroed | positionedTile

    updateFamily(tile.family, newFamily)
  }

  private def updateFamily(family: Family, newFamily: BitFamily) = family match {
    case Bamboo => copy(bamboos = newFamily)
    case Character => copy(characters = newFamily)
    case Stone => copy(stones = newFamily)
    case f: HonorFamily => copy(honors = newFamily)
  }

  private def selectFamily[A](family: Family) = family match {
    case Bamboo => bamboos
    case Character => characters
    case Stone => stones
    case f: HonorFamily => honors
  }


  /**
   *
   * @param family
   * @return number of tiles in `family`
   */
  private def sizeOf(family: Int) = {
    (1 to 9).fold(0) {
      (somme: Int, i: Int) => somme + occurrenceOf(family, i)
    }
  }

  /**
   *
   * @param family
   * @param value
   * @return The number of occurrences for the tile with `value` in `family`
   */
  private def occurrenceOf(family: Int, value: Tile.TileValue) = {
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

  val oneOfEach = Integer.parseInt("111111111", 8)

}
