package org.liprudent.majiang.tiles

import org.liprudent.majiang.tiles.Types._
import org.liprudent.majiang.tiles.TocTileSet

/**
 * A TileSet is just a data structure that contains some tiles
 * <p/>
 * Methods are provided :
 * - to add and remove tiles
 * - to find useful properties of the set
 */

trait TileSet {

  /**
   * The number of tiles in the set
   */
  def size: Int

  /**
   * @param tile the tile to remove
   * @return a new TileSet with the tile removed
   */
  def removed(tile: Tile): TileSet

  /**
   * @param tiles tiles to remove
   * @return a new TileSet with the tiles removed
   */
  def removed(tiles: Seq[Tile]): TileSet = {
    tiles.foldLeft(this)((handAcc, tile) => handAcc.removed(tile))
  }

  /**
   *
   * @param tile The tile to add
   * @return a new TileSet with a new tile
   */
  def added(tile: Tile): TileSet

  /**
   *
   * @return true if all occurence of each tile in this set is 1
   */
  def isAllUnique : Boolean


}
