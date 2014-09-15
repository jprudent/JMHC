package org.liprudent.majiang

import net.liftweb.json.JsonAST.JValue
import net.liftweb.json.{Extraction, JsonAST, JsonParser, Printer}
import org.liprudent.majiang.figures.Kong
import org.liprudent.majiang.mahjong.{DetailedPoints, HuLe, HulePointsComputer}
import org.liprudent.majiang.tiles.{ContextualTile, PlayerContext, _}
import org.liprudent.majiang.ui.StringMapper

object HuLePointsComputerJsonAdapter {

  def wrapOrError(request: String): String = {
    //Why wrap(request).map(_).getOrElse("ERROR") doesn't work ???
    wrap(request).map(r => r).getOrElse("ERROR")
  }

  def wrap(request: String): Option[String] = {
    val json = JsonParser.parse(request)
    wrap(json)
  }

  def wrap(json: JValue): Option[String] = {

    implicit val formats = net.liftweb.json.DefaultFormats
    val req = json.extractOpt[Request]
    req.map(r => {
      val decompose = Extraction.decompose(CommonWrapper.toMap(r.compute))
      Printer.pretty(JsonAST.render(decompose))
    })
  }

  /**
   * Representation of the JSON object in Scala. Used to deserialising JSON request
   * @param concealed "b1-b1-b1 b2-b2"
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

    def compute: DetailedPoints = {

      def toFigures(figures: Option[String]) =
        figures.map(s => StringMapper.toFigures(StringMapper.splitFigures(s))).getOrElse(Nil)

      val closed = toFigures(Some(concealed))

      //      val concealed_tiles = StringMapper.toTiles(StringMapper.splitTiles(concealed))
      val winningTile = Tile(winning_tile)
      val winningTileOrigin = TileOrigin(winning_tile_origin)
      val lastTileSituation = last_tile_situation.map(LastTileSituation(_)).getOrElse(NotLastTile)

      val concealedKong = toFigures(concealed_kong).asInstanceOf[List[Kong]]
      val melded_figures = toFigures(melded)
      val winningSituation = ContextualTile(winningTile, winningTileOrigin, lastTileSituation)

      def toWind(wop: Option[String]) = wop.map(WindFamily(_)).getOrElse(EastWind)

      val prevalentW = toWind(prevalent_wind)
      val playerW = toWind(player_wind)
      val playerContext: PlayerContext = PlayerContext(playerW, prevalentW)

      val waitingTiles = UniqueWait.waitingTiles(TileSet(closed.map(_.toTiles).flatten), melded_figures, concealedKong)

      val huLe: HuLe = HuLe(closed, melded_figures, winningSituation, playerContext, waitingTiles, concealedKong)

      HulePointsComputer(huLe)
    }

  }

}
