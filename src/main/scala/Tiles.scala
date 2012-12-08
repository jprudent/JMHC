package org.liprudent.majiang

package object tiles {

  //////////////////////////////////////////////////////////////////////
  // TILES DEFINITION
  //

  sealed abstract class Family {
    def name: String

    def validValue(v: Int): Boolean

    def order: Int
  }

  object Family {
    implicit val ord = new Ordering[Family] {
      def compare(f1: Family, f2: Family) = {
        f1.order.compareTo(f2.order)
      }
    }
  }

  sealed abstract class SuitFamily extends Family {
    override def validValue(value: Int) = value >= 1 && value <= 9
  }

  case object Bamboo extends SuitFamily {
    override val name = "Bamboo"
    override val order = 0;
  }

  case object Stone extends SuitFamily {
    override val name = "Stone"
    override val order = 2;
  }

  case object Character extends SuitFamily {
    override val name = "Character"
    override val order = 1;
  }

  sealed abstract class HonorFamily extends Family {
    override def validValue(value: Int) = value == 0xF00
  }

  case object EastWind extends HonorFamily {
    override val name = "East Wind"
    override val order = 3;
  }

  case object WestWind extends HonorFamily {
    override val name = "West Wind"
    override val order = 5;
  }

  case object NorthWind extends HonorFamily {
    override val name = "North Wind"
    override val order = 4;
  }

  case object SouthWind extends HonorFamily {
    override val name = "South Wind"
    override val order = 6;
  }

  case object RedDragon extends HonorFamily {
    override val name = "Red Dragon"
    override val order = 7;
  }

  case object GreenDragon extends HonorFamily {
    override val name = "Green Dragon"
    override val order = 8;
  }

  case object WhiteDragon extends HonorFamily {
    override val name = "White Dragon"
    override val order = 9;
  }

  class Tile private(val family: Family, val value: Int) {

    // a tile value should be between 1 and 9
    require(family.validValue(value))

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

    def apply(family: SuitFamily, value: Int) = new Tile(family, value)

    def apply(family: HonorFamily) = new Tile(family, 0xF00)

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

    val b1 = Tile(Bamboo, 1)
    val b2 = Tile(Bamboo, 2)
    val b3 = Tile(Bamboo, 3)
    val b4 = Tile(Bamboo, 4)
    val b5 = Tile(Bamboo, 5)
    val b6 = Tile(Bamboo, 6)
    val b7 = Tile(Bamboo, 7)
    val b8 = Tile(Bamboo, 8)
    val b9 = Tile(Bamboo, 9)
    val c1 = Tile(Character, 1)
    val c2 = Tile(Character, 2)
    val c3 = Tile(Character, 3)
    val c4 = Tile(Character, 4)
    val c5 = Tile(Character, 5)
    val c6 = Tile(Character, 6)
    val c7 = Tile(Character, 7)
    val c8 = Tile(Character, 8)
    val c9 = Tile(Character, 9)
    val s1 = Tile(Stone, 1)
    val s2 = Tile(Stone, 2)
    val s3 = Tile(Stone, 3)
    val s4 = Tile(Stone, 4)
    val s5 = Tile(Stone, 5)
    val s6 = Tile(Stone, 6)
    val s7 = Tile(Stone, 7)
    val s8 = Tile(Stone, 8)
    val s9 = Tile(Stone, 9)
    val we = Tile(EastWind)
    val ww = Tile(WestWind)
    val ws = Tile(SouthWind)
    val wn = Tile(NorthWind)
    val dr = Tile(RedDragon)
    val dw = Tile(WhiteDragon)
    val dg = Tile(GreenDragon)

  }


  ///////////////////////////////////////////////////////////////////////
  // TILES FIGURES DEFINITION

  type Suit = List[Tile]

  sealed trait FigureProperties {
    val size: Int
  }

  sealed trait Figure {
    val properties: FigureProperties
  }



  case class Chow(t1: Tile, t2: Tile, t3: Tile) extends Figure {

    def this(xs: List[Tile]) {
      this(xs(0), xs(1), xs(2))
      require(xs.length == 3)
    }

    val properties = ChowProperties
  }

  object ChowProperties extends Ordering[Chow] with FigureProperties {

    val size = 3

    def compare(chow1: Chow, chow2: Chow) = Tile.ord.compare(chow1.t1, chow2.t1)

  }



  case class Dui(t: Tile) extends Figure {
    val properties = DuiProperties
  }

  object DuiProperties extends Ordering[Dui] with FigureProperties {

    val size = 2

    def compare(dui1: Dui, dui2: Dui) = Tile.ord.compare(dui1.t, dui2.t)

  }



  case class Pung(t: Tile) extends Figure {
    val properties = PungProperties
  }

  object PungProperties extends Ordering[Pung] with FigureProperties {

    val size = 3

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
     * Implementation note:
     * Comparison is done on the first tile of each list.
     */
    def compare(x: List[(Tile, Occurence)], y: List[(Tile, Occurence)]): Int = {
      if (x.isEmpty && y.isEmpty) 0
      else if (x.isEmpty) -1
      else if (y.isEmpty) 1
      else ordTileOccurence.compare(x(0), y(0))
    }
  }


  case class Hand(hand: List[TileOccurence]) {

    require(hand.forall {
      to => to._2 >= 1 && to._2 <= 4
    })

    lazy val size: Int = hand.foldLeft(0)((sum: Int, toc: TileOccurence) => sum + toc._2)

    /**
     * an ordered list of possible pungs
     */
    lazy val findPungs: List[Pung] = {
      hand.filter(t => t._2 >= PungProperties.size).map {
        t => new Pung(t._1)
      }
    }

    /**
     * an ordered list of possible chows
     */
    lazy val findChows: List[Chow] = {
      listsOf(ChowProperties.size, allSuits).map(new Chow(_))
    }

    /**
     * an ordered list of possible duis
     */
    lazy val findDuis: List[Dui] = {
      hand.filter(t => t._2 >= DuiProperties.size).map {
        t => {
          val d = new Dui(t._1)
          if (t._2 == 4) List(d,d)
          else List(d)
        }
      }.flatten
    }

    /**
     * an ordered list of possible length free suits
     */
    lazy val allSuits: List[Suit] = {
      splitByFamily.map {
        tilesSameFamily =>
          findSuits(tilesSameFamily)
      }.flatten
    }

    /**
     * a list of sub-list containing ordered TileOccurence of the same family
     */
    lazy val splitByFamily: List[List[TileOccurence]] = {
      hand.groupBy {
        e => e._1.family
      }.values.toList.sorted
    }

    def remove(t: Tile): Hand = {
      val removed: List[_root_.org.liprudent.majiang.tiles.TileOccurence] = remove(t, hand)
      new Hand(removed)
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
          else ((tile, occ - 1) :: from.filterNot(isTile)).sorted
      }
    }

    def remove(tiles: List[Tile], from: List[TileOccurence]): List[TileOccurence] = {
      tiles.foldLeft(from)((acc: List[TileOccurence], t: Tile) => remove(t, acc))
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


    def listsOf(suitSize: Int, suits: List[Suit]): List[Suit] = {
      require(suitSize > 0)

      def listsOf(suitSize: Int, suit: Suit): List[Suit] = {
        findSubSuitsIndices(suit.size, suitSize).map {
          indices: List[Int] =>
            indices.map(i => suit(i))
        }
      }

      suits.map(listsOf(suitSize, _)).flatten
    }


    /**
     * Computes all fixed <code>length</code> sub-suit indices of a suit of tiles which length is
     * <code>suitSize</code>.
     * @param suitSize The list where to compute sub-lists indices from
     * @param length The fixed length of each sub-suit
     * @return a list of list of indices. Indices are sorted ascending and lists too.
     *
     **/
    def findSubSuitsIndices(suitSize: Int, length: Int): List[List[Int]] = {
      val nbElements = suitSize - length + 1
      (for {i <- 0 until nbElements} yield (for {j <- i until i + length} yield j).toList).toList
    }


    override def toString: String = "Hand : " + hand.toString

  }

  object Hand {

    def apply(tiles: List[Tile])(implicit notUsed:DummyImplicit): Hand = {
      val hand = tiles.groupBy(t => t).map {
        case (k, v) => (k, v.size)
      }.toList.sorted
      new Hand(hand)
    }

  }


}
