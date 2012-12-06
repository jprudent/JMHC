package org.liprudent.majiang

package object tiles {

  //////////////////////////////////////////////////////////////////////
  // TILES DEFINITION
  //

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

    def nextOf(tile: Tile) = tile.previousOf(this)

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
        if (compFamily == 0) t1.value.compare(t2.value)
        else compFamily
      }

    }

  }


  ///////////////////////////////////////////////////////////////////////
  // TILES ARRANGEMENTS DEFINITION

  type Suit = List[Tile]

  case class Chow(t1: Tile, t2: Tile, t3: Tile)

  /* FIXME : I would like to be able to sort a List[Chow]. Why the method
	`sorted` doesn't work out of the box and I do have to create a companion
	object ? */

  object Chow extends Ordering[Chow] {
    def compare(chow1: Chow, chow2: Chow) = Tile.ord.compare(chow1.t1, chow2.t1)

    def apply(xs: List[Tile]) = {
      require(xs.length == 3)
      new Chow(xs(0), xs(1), xs(2))
    }
  }

  case class Pung(t: Tile)

  object Pung extends Ordering[Pung] {
    def compare(pung1: Pung, pung2: Pung) = Tile.ord.compare(pung1.t, pung2.t)
  }


  ///////////////////////////////////////////////////////////////////////
  // HAND DEFINITIONS

  type Occurence = Int

  type TileOccurence = (Tile, Occurence)


  implicit val ordTileOccurence = new Ordering[TileOccurence] {

    def compare(t1: (Tile, Occurence), t2: (Tile, Occurence)): Int = {
      Tile.ord.compare(t1._1, t2._1)
    }
  }

  implicit val ordListTileOccurence = new Ordering[List[TileOccurence]] {

    /**
     * Comparison is done on the first tile of each list.
     * @param x
     * @param y
     * @return
     */
    def compare(x: List[(Tile, Occurence)], y: List[(Tile, Occurence)]): Int = {
      if (x.isEmpty && y.isEmpty) 0
      else if (x.isEmpty) -1
      else if (y.isEmpty) 1
      else ordTileOccurence.compare(x(0), y(0))
    }
  }


  case class Hand private(val hand: List[TileOccurence]) {

    def remove(t: Tile): Hand = {
      Hand(remove(t, hand))
    }

    /**
     * Remove a tile from list of tiles.
     * @param t The tile to remove
     * @param from The list where to remove the tile <code>t</code>
     * @return The list <code>from</code> minus the tile <code>t</code>
     */
    def remove(t: Tile, from: List[TileOccurence]): List[TileOccurence] = {

      def isTile = (tileOccurence: TileOccurence) => tileOccurence._1 == t

      from.find(isTile) match {
        case None => from
        case Some((tile, occ)) =>
          if (occ == 1) from.filterNot(isTile)
          else ((tile, occ - 1) :: from.filterNot(isTile)) sorted
      }
    }

    def remove(tiles: List[Tile], from: List[TileOccurence]): List[TileOccurence] = {
      tiles.foldLeft(from)((acc: List[TileOccurence], t: Tile) => remove(t, acc))
    }

    /**
     *
     * @return a list of <code>List[TileOccurence]</code>.
     *         Each list element contains tiles of the same family
     */
    def splitByFamily: List[List[TileOccurence]] = {
      hand.groupBy {
        e => e._1.family
      }.values.toList.sorted
    }


    /**
     * Search for the longest suits.
     * @param tiles where to find suits.
     * @return the list of the longest suits
     */
    def findSuits(tiles: List[TileOccurence]): List[Suit] = {

      def findSuit(tiles: List[TileOccurence], prev: Tile): Suit = {
        tiles match {
          case h :: t if prev.previousOf(h._1) => prev :: findSuit(t, h._1)
          case _ => List(prev)
        }
      }

      tiles match {
        case Nil => Nil
        case h :: t => {
          val suit = findSuit(t, h._1)
          suit :: findSuits(remove(suit, tiles))
        }
      }

    }


    private def listsOf(suitSize: Int, suits: List[Suit]): List[Suit] = {
      require(suitSize > 0)
      suits.map {
        suit =>
          findCombination(suit.size, suitSize).map {
            indices: List[Int] =>
              indices.map(i => suit(i))
          }
      }.flatten
    }


    def findChows: List[Chow] = {

      val allSuits = splitByFamily.map {
        tilesSameFamily =>
          findSuits(tilesSameFamily)
      }.flatten

      listsOf(3, allSuits).map(Chow(_))

    }

    def findCombination(suit: Int, size: Int): List[List[Int]] = {
      suit match {
        case i if i < size => Nil
        case _ => (suit until (suit - size) by -1).toList ::
          findCombination(suit - 1, size)
      }
    }


    /**
     * return an ordered list of possible pungs
     *
     */
    def findPungs: List[Pung] = {
      hand.filter(t => t._2 >= 3).map {
        t => Pung(t._1)
      }
    }

    override def toString: String = "Hand : " + hand.toString

  }

  object Hand {

    def apply(tiles: List[Tile], foo: Any = 0): Hand = {
      val hand = tiles.groupBy(t => t).map {
        case (k, v) => (k, v.size)
      }.toList.sorted
      new Hand(hand)
    }

  }


}
