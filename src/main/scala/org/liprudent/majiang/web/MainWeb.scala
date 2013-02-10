package org.liprudent.majiang.web

import unfiltered.request._
import unfiltered.response._
import net.liftweb.json.JsonAST.JValue
import org.liprudent.majiang.ui.StringMapper
import org.liprudent.majiang.HuFinder
import org.liprudent.majiang.tiles._
import org.liprudent.majiang.mahjong.PlayerContext
import org.liprudent.majiang.mahjong.PlayerTiles
import org.liprudent.majiang.tiles.ContextualTile
import scala.Some
import unfiltered.response.ResponseString
import org.liprudent.majiang.figures.Kong

object MainWeb {
  val api = unfiltered.filter.Intent {
    case GET(Path("/api")) => ResponseString(
      """
        | -- TILES --
        | there is a 2 characters length string available to describe any tile
        |  *) Bamboos    : b1 b2 b3 b4 b5 b6 b7 b8 b9  [ b(amboo)                                           ]
        |  *) Characters : c1 c2 c3 c4 c5 c6 c7 c8 c9  [ c(haracter)                                        ]
        |  *) Stones     : s1 s2 s3 s4 s5 s6 s7 s8 s9  [ s(tone)                                            ]
        |  *) Honors     : dg dr dw                    [ d(ragon) g(reen)  r(ed)    w(hite)                 ]
        |  *) Winds      : we wn ww ws                 [ w(ind)   e(ast)   n(orth)  w(est)   s(outh)        ]
        |  *) Flowers    : fp fo fc fb                 [ f(lower) p(lumb)  o(rchid) b(amboo) c(hrysantheum) ]
        |  *) Seasons    : ss su sa sw                 [ s(eason) s(pring) s(u)mmer a(utomn) w(inter)       ]
        |
        |  -- FIGURES --
        | You can associate tiles to form figures:
        |  *) Dui/Pair           : dg-dg       c1-c1       ...
        |  *) Chow/Straight/Suit : c1-c2-c3    s7-s8-s9    ...
        |  *) Pung/Triplet       : ww-ww-ww    dr-dr-dr    c1-c1-c1    ...
        |  *) Kong/Quartet       : ww-ww-ww-ww dr-dr-dr-dr c1-c1-c1-c1 ...
        |  *) Flowers/Bonus      : fp-ss-sw    ...
        | Some figures are invalid like: dg-dr c1-c2-c4 c1-c2-c2 ww-ww-ws ...
        | You can describe a set of figures within double quotes (") separating each figure by a space
        | For instance : "dg-dg c1-c2-c3 dr-dr-dr ww-ww-ww-ww fp-ss" represents a pair of green dragon,
        |                a chow of 1-2-3 characters, a pung of red dragon, a kong of west wind and a
        |                2 tiles bonus (plumb flower and spring season)
        | Another example with a list of single tiles: "b1 b3 b2 b2 b2 b3 b1 b1 b3" which makes no
        | assumption of possible figures ("b1-b2-b3 b1-b2-b3 b1-b2-b3" or "b1-b1-b1 b2-b2-b2 b3-b3-b3").
        |
        | -- SERVICES --
        | *) POST /compute
        |    Request body as JSON. Header "application/json".
        |    {
        |
        |      /* Mandatory. At least two tiles. Not tied. Winning tile should be in that list.             */
        |      "concealed" : "b1 b3 b2 b2 b2 b3 b1 b1 b3",
        |
        |      /* Optional if empty. A list of kongs.                                                       */
        |      "concealed_kong" : "dr-dr-dr-dr b1-b1-b1-b1",
        |
        |      /* Optional if empty. A list of figures.                                                     */
        |      "melded" : "b1-b2-b3 b1-b2-b3 b1-b2-b3",
        |
        |      /* Mandatory should be in "concealed"                                                        */
        |      "winning_tile" : "b1",
        |
        |      /* Mandatory. Possible values are:                                                           */
        |      /*    - "Self Drawn"    : Origin of the tile when the player self drawn the tile.            */
        |      /*    - "Discarded"    : Origin of the tile when a foe discarded a tile and player use it to */
        |      /*                       declare Hu.                         "                               */
        |      /*    - "Kong Robbed"   : Origin of the tile when a foe declare a melded kong and the        */
        |      /*                       very tile that transforms the pung to a kong is robbed.             */
        |      /*    - "Replaced Tile" : Origin of the tile when player declared a kong and need a          */
        |      /*                       replacement tile. That replacement tile is used to declare Hu.      */
        |      "winning_tile_origin" : "KongRobbed",
        |
        |      /* Optional. Default is "NotLastTile". Possible values are :                                 */
        |      /*    - "Not Last Tile"    : This is not a last tile situation                               */
        |      /*    - "Last Tile Of Kind" : 3 tiles of a kind are visible and player finished with the 4th */
        |      /*    - "Last Tile Claim"  : Player declares Hu with the last discarded tile of the game     */
        |      "last_tile_situation" : "LastTileOfKind",
        |
        |      /* Optional. "EAST" | "NORTH" | "WEST" | "SOUTH". Default to "EAST".                         */
        |      "prevalent_wind" : "WEST",
        |
        |      /* Optional. "EAST" | "NORTH" | "WEST" | "SOUTH". Default to "EAST".                         */
        |      "player_wind" : "WEST"
        |
        |    }
      """.stripMargin)
  }

  val compute = unfiltered.filter.Intent {
    case req@POST(Path("/compute")) =>
      JsonBody(req) match {
        case Some(json) =>
          handleCompute(json).getOrElse(BadRequestWithBody("invalid json format, please consult /api"))
        case _ => BadRequestWithBody("not a valid Json")
      }
  }

  private object BadRequestWithBody {
    def apply(s: String) = BadRequest ~> ResponseString(s)
  }

  private def handleCompute(json: JValue) = {
    case class Request(concealed: String,
                       concealed_kong: Option[String],
                       melded: Option[String],
                       winning_tile: String,
                       winning_tile_origin: String,
                       last_tile_situation: Option[String],
                       prevalent_wind: Option[String],
                       player_wind: Option[String])

    implicit val formats = net.liftweb.json.DefaultFormats
    val req = json.extractOpt[Request]
    req.map {
      r =>

        def toFigures(figures: Option[String]) =
          figures.map(s => StringMapper.toFigures(StringMapper.splitFigures(s))).getOrElse(Nil)

        val concealed = StringMapper.toTiles(StringMapper.splitTiles(r.concealed))
        val winningTile = Tile(r.winning_tile)
        val winningTileOrigin = TileOrigin(r.winning_tile_origin)
        val lastTileSituation = r.last_tile_situation.map(LastTileSituation(_)).getOrElse(NotLastTile)
        val concealedTiles = ConcealedTiles(concealed, ContextualTile(winningTile, winningTileOrigin, lastTileSituation))

        val concealedKong = toFigures(r.concealed_kong).asInstanceOf[List[Kong]]
        val melded = toFigures(r.melded)
        val ptiles: PlayerTiles = PlayerTiles(concealedTiles, melded, concealedKong)

        def toWind(wop: Option[String]) = wop.map(WindFamily(_)).getOrElse(EastWind)
        val prevalentW = toWind(r.prevalent_wind)
        val playerW = toWind(r.player_wind)
        val pcontext: PlayerContext = PlayerContext(playerW, prevalentW)

        val res = HuFinder(ptiles, pcontext).find
        ResponseString(res.toString)


    }

  }

  val all = unfiltered.filter.Planify(
    api onPass compute
  )


  def main(args: Array[String]) {
    unfiltered.jetty.Http(8080).plan(all).run
  }
}
