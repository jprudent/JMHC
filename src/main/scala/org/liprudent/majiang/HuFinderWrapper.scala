package org.liprudent.majiang

import org.json4s._
import org.json4s.native.{JsonMethods, Printer, JsonParser}
import org.liprudent.majiang.ui.StringMapper
import org.liprudent.majiang.tiles._
import org.liprudent.majiang.figures.Kong
import org.liprudent.majiang.tiles.PlayerContext
import org.liprudent.majiang.tiles.PlayerTiles
import org.liprudent.majiang.mahjong.DetailedPoints
import org.liprudent.majiang.tiles.ContextualTile


object HuFinderWrapper {
  
  def wrapOrError(request:String) : String = {
    //Why wrap(request).map(_).getOrElse("ERROR") doesn't work ???
    wrap(request).map(r=>r).getOrElse("ERROR")
  }

  def wrap(request: String): Option[String] = {
    val json = JsonParser.parse(request)
    wrap(json)
  }

  def wrap(json: JValue): Option[String] = {

    implicit val formats = DefaultFormats
    val req = json.extractOpt[Request]
    req.map(r => {
      val decompose = Extraction.decompose(Result(r.compute))
      Printer.pretty(JsonMethods.render(decompose))
    })
  }

  /**
   * Representation of the JSON object in Scala. Used to deserialising JSON request
   * @param concealed
   * @param concealed_kong
   * @param melded
   * @param winning_tile
   * @param winning_tile_origin
   * @param last_tile_situation
   * @param prevalent_wind
   * @param player_wind
   */
  private case class Request(concealed: String,
                             concealed_kong: Option[String],
                             melded: Option[String],
                             winning_tile: String,
                             winning_tile_origin: String,
                             last_tile_situation: Option[String],
                             prevalent_wind: Option[String],
                             player_wind: Option[String]) {

    def compute: List[DetailedPoints] = {

      def toFigures(figures: Option[String]) =
        figures.map(s => StringMapper.toFigures(StringMapper.splitFigures(s))).getOrElse(Nil)

      val concealed_tiles = StringMapper.toTiles(StringMapper.splitTiles(concealed))
      val winningTile = Tile(winning_tile)
      val winningTileOrigin = TileOrigin(winning_tile_origin)
      val lastTileSituation = last_tile_situation.map(LastTileSituation(_)).getOrElse(NotLastTile)
      val concealedTiles = ConcealedTiles(concealed_tiles, ContextualTile(winningTile, winningTileOrigin, lastTileSituation))

      val concealedKong = toFigures(concealed_kong).asInstanceOf[List[Kong]]
      val melded_figures = toFigures(melded)
      val ptiles: PlayerTiles = PlayerTiles(concealedTiles, melded_figures, concealedKong)

      def toWind(wop: Option[String]) = wop.map(WindFamily(_)).getOrElse(EastWind)
      val prevalentW = toWind(prevalent_wind)
      val playerW = toWind(player_wind)
      val pcontext: PlayerContext = PlayerContext(playerW, prevalentW)

      HuFinder(ptiles, pcontext).find
    }

  }

  private object Result {
    def apply(res: List[DetailedPoints]) = {
      res match {
        case Nil => Map(("message", "no mahjong found"))
        case h :: t => CommonWrapper.toMap(h)
      }
    }
  }

}
