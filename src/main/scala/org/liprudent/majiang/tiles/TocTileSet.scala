package org.liprudent.majiang.tiles

import org.liprudent.majiang.tiles.Types._
import scala.Some

/**
 * A TileSet is just a data structure that contains some tiles
 * <p/>
 * Methods are provided to add and remove tiles
 * @param tocs The list of tiles this class handles
 */
case class TocTileSet(private val tocs: List[TileOccurence]) extends TileSet{

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
  def removed(tile: Tile): TocTileSet = {
    val removed: List[TileOccurence] = remove(tile, tocs)
    TocTileSet(removed)
  }

  /**
   * a list of tiles removed
   * @param tiles
   * @return a new TileSet with the tiles removed
   */
  def removed(tiles: List[Tile]): TocTileSet = {
    tiles.foldLeft(this)((handAcc, tile) => handAcc.removed(tile))
  }

  def added(tile: Tile): TocTileSet = {
    val added = add(tile, tocs)
    TocTileSet(added)
  }

  lazy val isAllUnique = ! toTiles.exists((tile: Tile) => occurence(tile) > 1)

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
  lazy val splitByFamily: List[List[TileOccurence]] = {
    tocs.groupBy {
      e => e._1.family
    }.values.toList.sorted(OrdListTileOccurence)
  }


  lazy val allTripletsOrQuadruplets: List[Tile] = findTilesWhereOcc(3)

  lazy val allQuadruplets: List[Tile] = findTilesWhereOcc(4)

  /**
   * @return all the tiles that can be used to form a pair.
   *         if a tile has 4 occurrences, then it'll be twice in result
   */
  lazy val allPairs: List[Tile] = {
    (findTilesWhereOcc(2) ::: allQuadruplets).sorted
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
   * @return All honor tiles
   */
  lazy val allHonors: List[Tile] = toTiles.filter(_.isHonor)

  /**
   * @return All straight tiles
   */
  lazy val allStraights: List[Tile] = toTiles.filter(_.isStraight)

  /**
   *
   * @param other
   * @return true if `this` is subset of of other
   */
  def isSubsetOf(other: TocTileSet): Boolean =
    toTiles.intersect(other.toTiles) == toTiles

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

  private def findTilesWhereOcc(minOccurence: Int): List[Tile] =
    tocs.filter(t => t._2 >= minOccurence).map(_._1)

}

object TocTileSet {
  def apply(tiles: Iterable[Tile])(implicit dummy: DummyImplicit): TocTileSet = {
    val tocs = tiles.groupBy(t => t).map {
      case (k, v) => (k, v.size)
    }.toList.sorted

    TocTileSet(tocs)
  }

}