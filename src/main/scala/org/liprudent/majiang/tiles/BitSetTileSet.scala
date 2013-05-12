package org.liprudent.majiang.tiles


case class BitSetTileSet(bamboos: Int) {

  import BitSetTileSet._

  val size:Int = sizeOf(bamboos);

  def removed(tile: Tile): BitSetTileSet = {
    require(tile.family == Bamboo, "only implemented on bamboos")

    // then shift it at the end, so it's like 00000000X (base 8)
    val tileOccurence = occurenceOf(bamboos,tile.value)
    assert((tileOccurence & maskForValue.last) == tileOccurence, Integer.toString(tileOccurence,8))

    // then we can remove one if it's not zero (negatives would ruin everything nah mean)
    val newTileOccurence = if (tileOccurence != 0) tileOccurence -1 else tileOccurence

    // move the new occurence to original place. Like 0000X0000 (base 8)
    val positionedTile = newTileOccurence << ((9 - tile.value) * 3)

    // zeroes tiles in original family: 111102222
    val zeroed = bamboos & inversedMaskForValue(tile.value - 1)

    // finally mix the zeroed with the original
    val newBamboos = zeroed | positionedTile

    this.copy(bamboos = newBamboos)
  }

  /**
   *
   * @param family
   * @return number of tiles in `family`
   */
  private def sizeOf(family: Int) = {
    (0 until 9).fold(0){
      (somme: Int, i: Int) => somme + sizeOfLast(family >>> (3*i))
    }
  }

  /**
   *
   * @param family
   * @return the number of tiles of the last tile of the family
   */
  private def sizeOfLast(family: Int) =
    family & maskForValue.last

  /**
   *
   * @param family
   * @param value
   * @return The number of occurences for the tile with `value` in `family`
   */
  private def occurenceOf(family: Int, value : Tile.TileValue) = {
    // zeroes everything except tile
    val onlyTile = family & maskForValue(value -1)
    // then shift it at the end, so it's like 00000000X (base 4)
    onlyTile >>> ((9-value)*3)
  }

}

object BitSetTileSet {

  val maskForValue  = Array(
    Integer.parseInt("700000000",8),
    Integer.parseInt("070000000",8),
    Integer.parseInt("007000000",8),
    Integer.parseInt("000700000",8),
    Integer.parseInt("000070000",8),
    Integer.parseInt("000007000",8),
    Integer.parseInt("000000700",8),
    Integer.parseInt("000000070",8),
    Integer.parseInt("000000007",8))

  val inversedMaskForValue = maskForValue.map(_ ^ Integer.parseInt("777777777",8))

}
