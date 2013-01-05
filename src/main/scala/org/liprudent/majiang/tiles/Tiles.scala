package org.liprudent.majiang.tiles

import Types.Occurence
import Types.TileOccurence
import Types.Figures

import org.liprudent.majiang.figures._


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

//TODO rename as StraightFamily
sealed abstract class SuitFamily extends Family {
  override def validValue(value: Int) = value >= 1 && value <= 9
}

case object Bamboo extends SuitFamily {
  override val name = "Bamboo"
  override val order = 0
}

case object Character extends SuitFamily {
  override val name = "Character"
  override val order = 1
}

case object Stone extends SuitFamily {
  override val name = "Stone"
  override val order = 2
}


sealed abstract class HonorFamily extends Family {
  override def validValue(value: Int) = value == 0xF00D

  def shortName: String
}

sealed abstract class WindFamily extends HonorFamily {}

sealed abstract class DragonFamily extends HonorFamily {}

case object EastWind extends WindFamily {
  override val name = "East Wind"
  override val order = 3
  override val shortName = "we"
}

case object WestWind extends WindFamily {
  override val name = "West Wind"
  override val order = 5
  override val shortName = "ww"
}

case object NorthWind extends WindFamily {
  override val name = "North Wind"
  override val order = 4
  override val shortName = "wn"
}

case object SouthWind extends WindFamily {
  override val name = "South Wind"
  override val order = 6
  override val shortName = "ws"
}

case object RedDragon extends DragonFamily {
  override val name = "Red Dragon"
  override val order = 7
  override val shortName = "dr"
}

case object GreenDragon extends DragonFamily {
  override val name = "Green Dragon"
  override val order = 8
  override val shortName = "dg"
}

case object WhiteDragon extends DragonFamily {
  override val name = "White Dragon"
  override val order = 9
  override val shortName = "dw"
}

sealed abstract class BonusFamily extends Family {
  override def validValue(value: Int) = value == 0xBABA

  def shortName: String
}

case object PlumbFlower extends BonusFamily {
  override val name = "Plumb Flower"
  override val order = 10
  override val shortName = "fp"
}

case object OrchidFlower extends BonusFamily {
  override val name = "Orchid Flower"
  override val order = 11
  override val shortName = "fo"
}

case object ChrysanthemumFlower extends BonusFamily {
  override val name = "Chrysanthemum Flower"
  override val order = 12
  override val shortName = "fc"
}

case object BambooFlower extends BonusFamily {
  override val name = "Bamboo Flower"
  override val order = 13
  override val shortName = "fb"
}

case object SpringSeason extends BonusFamily {
  override val name = "Spring Season"
  override val order = 14
  override val shortName = "ss"
}


case object SummerSeason extends BonusFamily {
  override val name = "Summer Season"
  override val order = 15
  override val shortName = "su"
}


case object AutomnSeason extends BonusFamily {
  override val name = "Automn Season"
  override val order = 16
  override val shortName = "sa"
}


case object WinterSeason extends BonusFamily {
  override val name = "Winter Season"
  override val order = 17
  override val shortName = "sw"
}

case class Tile(family: Family, value: Int) {

  // a tile value should be between 1 and 9
  require(family.validValue(value))

  lazy val isStraight = family.isInstanceOf[SuitFamily]

  lazy val isTerminal = isStraight && (value == 9 || value == 1)

  lazy val isHonor = family.isInstanceOf[HonorFamily]

  lazy val isTerminalOrHonor = isTerminal || isHonor

  lazy val isBonus = family.isInstanceOf[BonusFamily]

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

  override def toString = family match {
    case t: SuitFamily => family.name.substring(0, 1).toLowerCase + value
    case t: HonorFamily => t.shortName
    case t: BonusFamily => t.shortName
  }
}

object Tile {

  def apply(family: SuitFamily, value: Int) = new Tile(family, value)

  def apply(family: HonorFamily) = new Tile(family, 0xF00D)

  def apply(family: BonusFamily) = new Tile(family, 0xBABA)

  implicit val ord = new Ordering[Tile] {

    /**
     * First criteria of compare is the family, then the value
     */
    def compare(t1: Tile, t2: Tile) = {
      val compFamily = Family.ord.compare(t1.family, t2.family)
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
  val fp = Tile(PlumbFlower)
  val fo = Tile(OrchidFlower)
  val fc = Tile(ChrysanthemumFlower)
  val fb = Tile(BambooFlower)
  val ss = Tile(SpringSeason)
  val su = Tile(SummerSeason)
  val sa = Tile(AutomnSeason)
  val sw = Tile(WinterSeason)

  val allButBonus = Set(
    b1, b2, b3, b4, b5, b6, b7, b8, b9,
    c1, c2, c3, c4, c5, c6, c7, c8, c9,
    s1, s2, s3, s4, s5, s6, s7, s8, s9,
    we, ww, ws, wn,
    dr, dw, dg)

}

trait TileOrigin

case object SelfDrawn extends TileOrigin

case object Discarded extends TileOrigin

case object KongStole extends TileOrigin

case class ContextualTile(tile: Tile, origin: TileOrigin, isLastTile: Boolean)

///////////////////////////////////////////////////////////////////////
// HAND DEFINITIONS


object OrdTileOccurence extends Ordering[TileOccurence] {

  def compare(t1: (Tile, Occurence), t2: (Tile, Occurence)): Int = {
    Tile.ord.compare(t1._1, t2._1)
  }
}

object OrdListTileOccurence extends Ordering[List[TileOccurence]] {

  /**
   * Implementation note:
   * Comparison is done on the first tile of each list.
   */
  def compare(x: List[(Tile, Occurence)], y: List[(Tile, Occurence)]): Int = {
    if (x.isEmpty && y.isEmpty) 0
    else if (x.isEmpty) -1
    else if (y.isEmpty) 1
    else OrdTileOccurence.compare(x(0), y(0))
  }
}

/**
 * A TileSet is just a data structure that contains some tiles
 * <p/>
 * Methods are provided to add and remove tiles
 * @param tocs The list of tiles this class handles
 */
//TODO tocs is a way of representing tiles internally in TileSet. So this implementation detail should be masked from API
//TODO action => tocs should be private
case class TileSet(tocs: List[TileOccurence]) {

  require(tocs.forall {
    to => to._2 >= 1 && to._2 <= 4
  })

  /**
   * The size of the data structure
   */
  lazy val size: Int = tocs.foldLeft(0)((sum: Int, toc: TileOccurence) => sum + toc._2)

  /**
   * a tile removed
   * @param tile the tile to remove
   * @return a new TileSet with the tile removed
   */
  def removed(tile: Tile): TileSet = {
    val removed: List[TileOccurence] = remove(tile, tocs)
    TileSet(removed)
  }

  /**
   * a list of tiles removed
   * @param tiles
   * @return a new TileSet with the tiles removed
   */
  def removed(tiles: List[Tile]): TileSet = {
    tiles.foldLeft(this)((handAcc, tile) => handAcc.removed(tile))
  }

  def added(tile: Tile): TileSet = {
    val added = add(tile, tocs)
    TileSet(added)
  }

  /**
   *
   * @param p the predicate used to test elements.
   * @return true if the given predicate p holds for some of the elements, otherwise false.
   *
   */
  def exists(p: (Tile) => Boolean) = toTiles.exists(p)

  def filter(p: (Tile) => Boolean): TileSet = TileSet(toTiles.filter(p))

  /**
   * a list of tilesets where tiles are all the sameof the same family
   */
  lazy val sameFamily: Boolean = {
    tocs match {
      case Nil => true
      case t :: ts => ts.forall(_._1.family == t._1.family)
    }
  }

  def occurence(tile: Tile): Occurence =
    tocs.find(_._1 == tile).map(_._2).getOrElse(0)

  lazy val toTiles: List[Tile] =
    tocs.map(toc => for {i <- 1 to toc._2} yield (toc._1)).flatten

  /**
   * Remove a tile from list of tiles.
   * @param t The tile to remove
   * @param from The list where to remove the tile <code>t</code>
   * @return The list <code>from</code> minus the tile <code>t</code>
   */
  protected def remove(t: Tile, from: List[TileOccurence]): List[TileOccurence] = {

    def isTile = (tileOccurence: TileOccurence) => tileOccurence._1 == t

    from.find(isTile) match {
      case None => throw new IllegalArgumentException("Tile " + t + " is not in " + toTiles)
      case Some((tile, occ)) =>
        if (occ == 1) from.filterNot(isTile)
        else ((tile, occ - 1) :: from.filterNot(isTile)).sorted
    }
  }

  protected def remove(tiles: List[Tile], from: List[TileOccurence]): List[TileOccurence] = {
    tiles.foldLeft(from)((acc: List[TileOccurence], t: Tile) => remove(t, acc))
  }


  /**
   * Add a tile to a list of tiles.
   * @param t The tile to add
   * @param to The list where to add the tile <code>t</code>
   * @return The list <code>to</code> plus the tile <code>t</code>
   */
  protected def add(t: Tile, to: List[TileOccurence]): List[TileOccurence] = {

    def isTile = (tileOccurence: TileOccurence) => tileOccurence._1 == t

    val added = to.find(isTile) match {
      case None => (t, 1) :: to
      case Some((tile, occ)) =>
        require(occ < 4 && occ >= 1, "There are " + occ + " " + tile)
        (tile, occ + 1) :: to.filterNot(isTile)
    }

    added.sorted
  }

}

object TileSet {
  def apply(tiles: Iterable[Tile])(implicit dummy: DummyImplicit): TileSet = {
    val tocs = tiles.groupBy(t => t).map {
      case (k, v) => (k, v.size)
    }.toList.sorted

    TileSet(tocs)
  }

  //  def apply(tile:Tile*) : TileSet = {
  //    apply(tile:_*)
  //  }
}

/**
 * This class will find all possibles set of figures that can be made with a set of tiles
 *
 * @param tileSet
 */
case class FiguresComputer(tileSet: TileSet) {


  /**
   * an ordered list of possible pungs
   */
  lazy val pungs: List[Pung] = {
    tileSet.tocs.filter(t => t._2 >= PungProperties.size).map {
      t => new Pung(t._1)
    }
  }

  /**
   * an ordered list of possible chows
   */
  lazy val chows: List[Chow] = {
    sublistsOf(Chow.size, allSuits).map(new Chow(_))
  }

  /**
   * an ordered list of possible duis
   */
  lazy val duis: List[Dui] = {
    tileSet.tocs.filter(t => t._2 >= DuiProperties.size).map {
      t => {
        val d = new Dui(t._1)
        if (t._2 == 4) List(d, d)
        else List(d)
      }
    }.flatten
  }

  lazy val knitted: List[Knitted] = {

    //quick fail
    if (tileSet.size < 9 || chows.size >= 2 || pungs.size >= 2) Nil

    else {
      List(Bamboo, Character, Stone)
        .permutations // all combinations of those 3 families
        .map(families => Knitted(families(0), families(1), families(2))).toList // as Knitted
        .filter(knitted => knitted.asList.forall(tile => tileSet.exists(t => t == tile))) // where each knitted tile exists in tileSet
        .toList
    }
  }

  lazy val someKnittedSomeDragons: List[SomeKnittedWithSomeDragons] = {

    //quick fail
    if (tileSet.size != 14) Nil

    else {
      val honors = tileSet.filter(!_.family.isInstanceOf[SuitFamily])
      if (hasTwins(honors) || honors.size < 5) {
        Nil
      }
      else {
        val knitted = tileSet.filter(_.family.isInstanceOf[SuitFamily])
        val knittedComputer: FiguresComputer = FiguresComputer(tileSet)
        if (knitted.size != 14 - honors.size || hasChowsOrTwins(knittedComputer)) {
          Nil
        }
        else {

          val tiles147 = knitted.filter(tile => tile.value == 1 || tile.value == 4 || tile.value == 7)
          val tiles258 = knitted.filter(tile => tile.value == 2 || tile.value == 5 || tile.value == 8)
          val tiles369 = knitted.filter(tile => tile.value == 3 || tile.value == 6 || tile.value == 9)
          val knitteds: List[TileSet] = List(tiles147, tiles258, tiles369)

          //all tiles should be the same family
          if (knitteds.forall(tileSet => tileSet.sameFamily)) {
            val allKnitted: List[Tile] = knitteds.map(_.toTiles).flatten.sorted
            assert(allKnitted.size == 14 - honors.size)
            List(SomeKnittedWithSomeDragons(allKnitted, honors.toTiles))
          } else {
            Nil
          }
        }
      }
    }
  }

  lazy val thirteenOrphans: List[ThirteenOrphans] = {
    val containsFixed = ThirteenOrphansProperties.fixedTiles.forall(tile => tileSet.exists(_ == tile))
    if (containsFixed) {
      val extra = tileSet.tocs.filter(toc => toc._2 == 2)
      extra match {
        case (tile, occ) :: Nil if tile.family.isInstanceOf[HonorFamily] => List(ThirteenOrphans(tile))
        case _ => Nil
      }
    }
    else {
      Nil
    }
  }

  private def hasTwins(tileSet: TileSet) =
    tileSet.tocs.forall(_._2 >= 2)

  private def hasChowsOrTwins(computer: FiguresComputer) =
    List(computer.chows, computer.pungs, computer.duis).exists(_.size != 0)

  /**
   * all possible figures
   * <p>
   * result is ordered : pungs, chows, duis
   */
  lazy val allFigures: List[Figure] = (thirteenOrphans ::: someKnittedSomeDragons ::: knitted ::: duis ::: chows ::: pungs).sorted(OrdFigure)

  /**
   *
   * @return All possible combinations of figures. Each element of the set is an ordered list of figures.
   */
  lazy val allFiguresCombinations: Set[Figures] = findFigures(this)

  //TODO optimization: it's useless to try all combinations for a given Figure type
  //List(Chow(b1,b2,b3), Chow(b2,b3,b4)) == List(Chow(b2,b3,b4), Chow(b1,b2,b3))
  protected def findFigures(figuresComputer: FiguresComputer): Set[Figures] = {
    figuresComputer.allFigures match {
      case Nil => Set()
      case all => {
        all.map {
          figure => {
            val next: Set[Figures] = findFigures(FiguresComputer(figuresComputer.tileSet.removed(figure.asList)))
            next match {
              case s if s.isEmpty => Set(List(figure))
              case _ => next.map(figures => (figure :: figures))
            }
          }
        }
          .flatten
          .toSet[Figures]
          .map(figures => figures.sorted(OrdFigure))
      }
    }
  }


  /**
   * an ordered list of possible length free suits
   */
  protected lazy val allSuits: List[Suit] = {
    splitByFamily.map {
      tilesSameFamily =>
        findSuits(TileSet(tilesSameFamily))
    }.flatten
  }

  /**
   * a list of sub-list containing ordered TileOccurence of the same family
   */
  protected[tiles] val splitByFamily: List[List[TileOccurence]] = {
    tileSet.tocs.groupBy {
      e => e._1.family
    }.values.toList.sorted(OrdListTileOccurence)
  }


  /**
   * Search for the longest suits.
   * @param tiles where to find suits.
   * @return the list of the longest suits
   */
  protected[tiles] def findSuits(tiles: TileSet): List[Suit] = {

    def findSuit(tiles: List[TileOccurence], prev: Tile): Suit = {
      tiles match {
        case h :: t if prev.previousOf(h._1) => prev :: findSuit(t, h._1)
        case _ => List(prev)
      }
    }

    tiles match {
      case TileSet(Nil) => Nil
      case TileSet(h :: t) => {
        val suit = findSuit(t, h._1)
        suit :: findSuits(tiles.removed(suit))
      }
    }

  }


  /**
   * Find all sublists of fixed length in given suits
   * @param suitSize sublists fixed length
   * @param suits the suits where to find sublists. Theses suits can have any length.
   * @return A list of sub-lists with fixed length
   */
  protected[tiles] def sublistsOf(suitSize: Int, suits: List[Suit]): List[Suit] = {
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
  protected[tiles] def findSubSuitsIndices(suitSize: Int, length: Int): List[List[Int]] = {
    val nbElements = suitSize - length + 1
    (for {i <- 0 until nbElements} yield (for {j <- i until i + length} yield j).toList).toList
  }
}

object FiguresComputer {
  def apply(tiles: List[Tile]): FiguresComputer = FiguresComputer(TileSet(tiles))
}

/**
 * A hand represents the closed tiles of a player. Those tiles can be rearranged, they are not melded.
 *
 * Note: There is no remove method because in mahjong a tile is removed only after one is added.
 * So if you need to remove a tile, call `addRemove` method.
 * @param tileSet The tiles the hand is made of.
 * @param lastTileContext The context of the last tile
 */
case class Hand(tileSet: TileSet, lastTileContext: ContextualTile) {

  require(tileSet.exists(t => t == lastTileContext.tile))

  /**
   * add a new tile and remove one
   * @param added ContextualTile to add
   * @param removed tile to remove
   * @return a new Hand with tiles updated
   */
  def addRemove(added: ContextualTile, removed: Tile): Hand = {
    Hand(add(added).tileSet.removed(removed), added)
  }

  /**
   * Add a new tile in hand
   *
   * @param contextualTile The new tile to add
   * @return The new hand with the new tile added
   */
  private def add(contextualTile: ContextualTile): Hand =
    Hand(tileSet.added(contextualTile.tile), contextualTile)

  override lazy val toString: String = "Hand : " + tileSet.toString + "\nLast tile: " + lastTileContext

}

object Hand {

  def apply(tiles: List[Tile], lastTileContext: ContextualTile)(implicit notUsed: DummyImplicit): Hand = {
    new Hand(TileSet(tiles), lastTileContext)
  }

}

