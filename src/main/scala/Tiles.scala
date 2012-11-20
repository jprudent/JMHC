package org.liprudent.majiang
object Tiles {

  sealed abstract class Family {
    def name: String
  }
  
  object Family {
    implicit val ord = new Ordering[Family] {
      def compare(f1: Family, f2: Family) = f1.name.compareTo(f2.name)
    }
  }

  case object Bamboo extends Family {
    override val name = "Bamboo"
  }

  case object Stone extends Family {
    override val name = "Stone"
  }

  case object Character extends Family {
    override val name = "Character"
  }

  class Tile(val family: Family, val value: Int) {

    // a tile value should be between 1 and 9
    require(value >= 1 && value <= 9)

    /**
     * return true if family is same
     */
    def sameFamily(tile: Tile) = tile.family == family

    /**
     * return true if the `tile` is the previous one in the same family
     * example: Tile(Bamboo,1).isPreviousOf(Tile(Bamboo,2)) should be true
     */
    def previousOf(tile: Tile) = sameFamily(tile) && tile.value == value + 1

    override def toString = value + "-" + family.name.substring(0, 3)
  }

  object Tile {

    def apply(family: Family, value: Int) = new Tile(family, value)

    implicit val ord = new Ordering[Tile] {

      /**
       * First criteria of compare is the family, then the value
       */
      def compare(t1: Tile, t2: Tile) = {
        var compFamily = Family.ord.compare(t1.family, t2.family) 
        if(compFamily == 0) t1.value.compare(t2.value)
        else compFamily
      }

    }

  }

  type Hand = List[Tile]

  case class Chow(t1: Tile, t2: Tile, t3: Tile)

  /* FIXME : I would like to be able to sort a List[Chow]. Why the method
	`sorted` doesn't work out of the box and I do have to create a companion
	object ? */

  object Chow extends Ordering[Chow] {
    def compare(chow1: Chow, chow2: Chow) = Tile.ord.compare(chow1.t1, chow2.t1)
  }

  /**
   * return an ordered list of possible chows without duplicates
   *
   * if hand has 1 2 3 bamboos twice, the result will contains 1 2 3 bamboos
   * only once whereas a valid hand may contains 1 2 3 bamboos twice
   */
  def findChows(tiles: Hand): List[Chow] =
    (for {
      a <- tiles
      b <- tiles.dropWhile(_ == a) if a.previousOf(b)
      c <- tiles.dropWhile(_ == b) if b.previousOf(c)
    } yield Chow(a, b, c)).toList sorted (Chow)


  case class Pung(t:Tile)
  
  object Pung extends Ordering[Pung] {
    def compare(pung1: Pung, pung2: Pung) = Tile.ord.compare(pung1.t, pung2.t)
  }

  /**
   * return an ordered list of possible pungs 
   *
   */
   def findPungs(tiles: Hand) : List[Pung] =  
     (for{
       a <- tiles
       b <- tiles.dropWhile(_ == a) if a == b
       c <- tiles.dropWhile(_ == b) if b == c
    } yield Pung(c)).toList sorted (Pung)

}
