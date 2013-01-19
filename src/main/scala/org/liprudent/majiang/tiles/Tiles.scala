package org.liprudent.majiang.tiles

import Types.Occurence
import Types.TileOccurence


//////////////////////////////////////////////////////////////////////
// TILES DEFINITION
//
sealed abstract class Family {
  def name: String

  def validValue(v: Int): Boolean

  def order: Int

  def shortName: String

  def <(f: Family) = Family.ord.lt(this, f)
}

object Family {
  implicit val ord = new Ordering[Family] {
    def compare(f1: Family, f2: Family) = {
      f1.order.compareTo(f2.order)
    }
  }

}

sealed abstract class StraightFamily extends Family {
  override def validValue(value: Int) = value >= 1 && value <= 9
}

object StraightFamily {
  val all = List(Bamboo, Character, Stone)
}

case object Bamboo extends StraightFamily {
  override val name = "Bamboo"
  override val order = 0
  override val shortName = "b"
}

case object Character extends StraightFamily {
  override val name = "Character"
  override val order = 1
  override val shortName = "c"
}

case object Stone extends StraightFamily {
  override val name = "Stone"
  override val order = 2
  override val shortName = "s"
}


sealed abstract class HonorFamily extends Family {
  override def validValue(value: Int) = value == 0xF00D
}

sealed abstract class WindFamily extends HonorFamily {
  def windName: String
}

object WindFamily {
  def apply(windName: String): WindFamily = {
    List(EastWind, NorthWind, WestWind, SouthWind).find(_.windName == windName).get
  }
}

sealed abstract class DragonFamily extends HonorFamily {}

case object EastWind extends WindFamily {
  override val name = "East Wind"
  override val order = 3
  override val shortName = "we"
  override val windName = "East"
}

case object WestWind extends WindFamily {
  override val name = "West Wind"
  override val order = 5
  override val shortName = "ww"
  override val windName = "West"
}

case object NorthWind extends WindFamily {
  override val name = "North Wind"
  override val order = 4
  override val shortName = "wn"
  override val windName = "North"
}

case object SouthWind extends WindFamily {
  override val name = "South Wind"
  override val order = 6
  override val shortName = "ws"
  override val windName = "South"
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

  lazy val isStraight = family.isInstanceOf[StraightFamily]

  lazy val isTerminal = isStraight && (value == 9 || value == 1)

  lazy val isHonor = family.isInstanceOf[HonorFamily]

  lazy val isWind = family.isInstanceOf[WindFamily]

  lazy val isDragon = family.isInstanceOf[DragonFamily]

  lazy val isFlower = family.isInstanceOf[BonusFamily]

  lazy val isTerminalOrHonor = isTerminal || isHonor

  lazy val isTerminalOrWind = isTerminal || isWind

  lazy val isFirst = value == 1

  lazy val isLast = value == 9

  lazy val isBonus = family.isInstanceOf[BonusFamily]

  /**
   * return true if family is same
   */
  def sameFamily(tile: Tile) = tile.family == family

  def sameValue(tile: Tile) = tile.value == value

  /**
   * return true if the `tile` is the previous one in the same family
   * example: Tile(Bamboo,1).isPreviousOf(Tile(Bamboo,2)) should be true
   */
  def previousOf(tile: Tile) = sameFamily(tile) && tile.value == value + 1

  lazy val next: Option[Tile] =
    if (isLast) None
    else Some(Tile(family, value + 1))

  def nextOf(tile: Tile) = tile.previousOf(this)

  def <(t: Tile) = Tile.ord.lt(this, t)

  override def toString = family match {
    case t: StraightFamily => family.name.substring(0, 1).toLowerCase + value
    case t: HonorFamily => t.shortName
    case t: BonusFamily => t.shortName
  }
}

object Tile {

  def apply(family: StraightFamily, value: Int) = new Tile(family, value)

  def apply(family: HonorFamily) = new Tile(family, 0xF00D)

  def apply(family: BonusFamily) = new Tile(family, 0xBABA)

  def apply(t: String): Tile = {
    val Straight = """([bcs])(\d)""".r
    val HonorAndFlowers = """(\w{2})""".r
    t match {
      case Straight(f, v) => Tile.all.find(tile => tile.family.shortName == f && tile.value == v.toInt).get
      case HonorAndFlowers(f) => Tile.all.find(_.family.shortName == f).get
    }
  }

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

  val all = Set(b1, b2, b3, b4, b5, b6, b7, b8, b9, c1, c2, c3, c4, c5, c6, c7, c8, c9, s1, s2, s3, s4, s5, s6, s7,
    s8, s9, we, ww, ws, wn, dr, dw, dg, fp, fo, fc, fb, ss, su, sa, sw)

  val allWind = Set(we, wn, ww, ws)

  val allDragons = Set(dr, dg, dw)

  val allBonus = Set(fp, fo, fc, fb, ss, su, sa, sw)

  val allButBonus = all -- allBonus

  val allHonors = allDragons ++ allWind

  val allStraight = allButBonus -- allHonors

  val allTerminalsOrHonors = allStraight.filter(_.isTerminalOrHonor)

}

/**
 * Origin of the last tile added in player's hand
 */
sealed trait TileOrigin

object TileOrigin {
  def apply(origin: String): TileOrigin = {
    origin.toUpperCase match {
      case "SELF DRAWN" => SelfDrawn
      case "DISCARDED" => Discarded
      case "KONG ROBBED" => KongRobbed
      case "REPLACED TILE" => ReplacedTile
    }
  }
}

/**
 * Origin of the tile when the player self drawn the tile
 */
case object SelfDrawn extends TileOrigin

/**
 * Origin of the tile when a foe discarded a tile and player use it to create a melded figure or declare Hu
 */
case object Discarded extends TileOrigin

/**
 * Origin of the tile when a foe declare a melded kong and the very tile that transforms the pung to a kong is robbed
 */
case object KongRobbed extends TileOrigin

/**
 * Origin of the tile when player declared a kong and need a replacement tile
 */
case object ReplacedTile extends TileOrigin

/**
 * Situation about last tile
 */
sealed trait LastTileSituation

/**
 * This is not a last tile situation
 */
case object NotLastTile extends LastTileSituation

/**
 * 3 tiles of a kind are visible and player finished with the fourth
 */
case object LastTileOfKind extends LastTileSituation

/**
 * Player declares Hu with the last discarded tile of the game
 */
case object LastTileClaim extends LastTileSituation

/**
 * Player declares Hu with the last tile of the wall
 */
case object LastTileDraw extends LastTileSituation

case class ContextualTile(tile: Tile, origin: TileOrigin, lastTileSituation: LastTileSituation) {

  require((lastTileSituation == LastTileClaim && origin == Discarded) || lastTileSituation != LastTileClaim,
    "when last tile situation is LastTileClaim, then origin is Discarded")

  require((lastTileSituation == LastTileDraw && origin == SelfDrawn) || lastTileSituation != LastTileDraw,
    "when last tile situation is LastTileDraw, then origin is SelfDrawn")
}

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
//TODO tocs is a way of representing tiles internally in TileSet. So this implementation detail should be masked from
// API
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

  lazy val allUnique = !exists((tile: Tile) => occurence(tile) > 1)

  /**
   * a list of tilesets where tiles are all the sameof the same family
   */
  lazy val isSingleFamily: Boolean = {
    tocs match {
      case Nil => true
      case t :: ts => ts.forall(_._1.family == t._1.family)
    }
  }

  /**
   * a list of sub-list containing ordered TileOccurence of the same family
   */
  val splitByFamily: List[List[TileOccurence]] = {
    tocs.groupBy {
      e => e._1.family
    }.values.toList.sorted(OrdListTileOccurence)
  }

  /**
   * Find the number of occurence of a given tile
   * @param tile Find number of occurence of this tile
   * @return the number of occurence of `tile`
   */
  def occurence(tile: Tile): Occurence =
    tocs.find(_._1 == tile).map(_._2).getOrElse(0)

  /**
   * convert as a list of Tile
   */
  lazy val toTiles: List[Tile] =
    tocs.map(toc => for {i <- 1 to toc._2} yield (toc._1)).flatten

  /**
   * Remove a tile from list of tiles.
   * @param t The tile to remove
   * @param from The list where to remove the tile <code>t</code>
   * @return The list <code>from</code> minus the tile <code>t</code>
   */
  private def remove(t: Tile, from: List[TileOccurence]): List[TileOccurence] = {

    def isTile = (tileOccurence: TileOccurence) => tileOccurence._1 == t

    from.find(isTile) match {
      case None => throw new IllegalArgumentException("Tile " + t + " is not in " + toTiles)
      case Some((tile, occ)) =>
        if (occ == 1) from.filterNot(isTile)
        else ((tile, occ - 1) :: from.filterNot(isTile)).sorted
    }
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

}

/**
 * A hand represents the closed tiles of a player. Those tiles can be rearranged, they are not melded.
 *
 * Note: There is no remove method because in mahjong a tile is removed only after one is added.
 * So if you need to remove a tile, call `addRemove` method.
 * @param tileSet The tiles the hand is made of.
 * @param lastTileContext The context of the last tile
 */
case class ConcealedTiles(tileSet: TileSet, lastTileContext: ContextualTile) {

  /**
   * add a new tile and remove one
   * @param added ContextualTile to add
   * @param removed tile to remove
   * @return a new ConcealedTiles with tiles updated
   */
  def addRemove(added: ContextualTile, removed: Tile): ConcealedTiles = {
    ConcealedTiles(add(added).tileSet.removed(removed), added)
  }

  /**
   * Add a new tile in hand
   *
   * @param contextualTile The new tile to add
   * @return The new hand with the new tile added
   */
  private def add(contextualTile: ContextualTile): ConcealedTiles =
    ConcealedTiles(tileSet.added(contextualTile.tile), contextualTile)

  override lazy val toString: String = "ConcealedTiles : " + tileSet.toString + "\nLast tile: " + lastTileContext

}

object ConcealedTiles {

  def apply(tiles: List[Tile], lastTileContext: ContextualTile)(implicit notUsed: DummyImplicit): ConcealedTiles = {
    new ConcealedTiles(TileSet(tiles), lastTileContext)
  }

}