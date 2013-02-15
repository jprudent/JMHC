package org.liprudent.majiang.ui

import org.liprudent.majiang.tiles.{Tile, Types}
import org.liprudent.majiang.figures._
import org.liprudent.majiang.figures.Dui

/**
 * This utility object transforms Strings to domain objects
 */
object StringMapper {

  /**
   *
   * @param tiles a list of figures. for instance "b2-b3-b4 dr-dr c1 c5"
   * @return a list of tiles. for instance List("b2","b3","b4","dr","dr","c1","c5")
   */
  def splitTiles(tiles: String) = tiles.split( """(\s|-)""")

  /**
   *
   * @param tiles a list of figures. for instance "b2-b3-b4 dr-dr c1 c5"
   * @return a list of tiles. for instance List("b2-b3-b4","dr-dr","c1","c5")
   */
  def splitFigures(tiles: String) = tiles.split( """(\s)""")

  /**
   *
   * @param tiles a list of strings. Each string representing a tile
   * @return a list of tiles
   * @example toTiles("b1"::"b2"::Nil) == b1::b2::Nil.toSeq
   */
  def toTiles(tiles: Seq[String]) = tiles.map(Tile(_)).toList

  def toFigures(figures: Seq[String]): Types.Figures = {

    def toFigure(figure: String): Figure = {

      def build(builder: (Tile) => Figure): Figure = {
        val tile = Tile(splitTiles(figure)(0))
        builder(tile)
      }

      def ??? = throw new RuntimeException(figure + " cannot be translated.")

      val tiles = splitTiles(figure).groupBy(t => t)
      tiles.size match {
        case 1 => tiles.values.head.size match {
          case 2 => build(Dui.apply)
          case 3 => build(Pung.apply)
          case 4 => build(Kong.apply)
          case _ => ???
        }
        case 3 => build(Chow.apply)
        case _ => ???
      }


    }

    figures.map(toFigure(_)).toList.sorted(OrdFigure)
  }
}
