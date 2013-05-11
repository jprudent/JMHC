package org.liprudent.majiang.tiles


case class BitSetTileSet(bamboos: Int) {

  import BitSetTileSet._

  val size:Int = sizeOf(bamboos);


  private def sizeOf(family: Int):Int = {
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
    family & lastMask
}

object BitSetTileSet {

  val lastMask = Integer.parseInt("7", 8)

}
