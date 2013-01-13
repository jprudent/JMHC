package org.liprudent.majiang.greenbook

import org.jbehave.core.steps.Steps
import org.jbehave.core.annotations._
import org.liprudent.majiang.tiles._
import org.liprudent.majiang.HuFinder
import org.liprudent.majiang.figures._
import org.liprudent.majiang.mahjong.PlayerContext
import org.liprudent.majiang.figures.Kong
import org.liprudent.majiang.figures.Pung
import org.liprudent.majiang.mahjong.PlayerTiles
import org.liprudent.majiang.figures.Dui
import org.liprudent.majiang.mahjong.DetailedPoints
import org.liprudent.majiang.tiles.ContextualTile

case class Context(
                    gnConcealed: List[Tile],
                    thConcealed: Types.Figures,
                    gnLastTileContext: ContextualTile,
                    gnMelded: Types.Figures,
                    gnPrevalentWind: WindFamily,
                    gnPlayerWind: WindFamily) {
  lazy val concealedTiles = ConcealedTiles(gnConcealed, gnLastTileContext)
  lazy val playerTiles = PlayerTiles(concealedTiles, gnMelded)
  lazy val playerContext = PlayerContext(gnPlayerWind, gnPrevalentWind)
}

object Context {

  val default = Context(
    gnConcealed = Nil,
    thConcealed = Nil,
    gnLastTileContext = ContextualTile(Tile.b7, Discarded, NotLastTile),
    gnMelded = Nil,
    gnPlayerWind = EastWind,
    gnPrevalentWind = NorthWind
  )

  // Context is stored in thread local because
  // jBehave instanciate steps clazz only once
  val instance = new ThreadLocal[Context]

  def get = instance.get()

  def set(ctx: Context) = instance.set(ctx)

  def init = instance.set(default)
}

object Result {
  val instance = new ThreadLocal[List[DetailedPoints]]

  def get = instance.get()

  def set(res: List[DetailedPoints]) {
    instance.set(res)
  }
}

class MahjongSteps extends Steps {

  /**
   * Handy accessor to result
   * @return result
   */
  def result = Result.get

  /**
   * Handy accessor to context
   * @return context
   */
  def context = Context.get


  @BeforeScenario
  def init() {
    Context.init
  }

  @Given( """concealed "$tiles"""")
  def givenConcealed(tiles: String) {
    val splitted = splitTiles(tiles)
    Context set context.copy(gnConcealed = splitted.map {
      tile => Tile(tile)
    }.toList.sorted(Tile.ord))
  }

  @Given( """melded "$tiles"""")
  def givenMelded(tiles: String) {
    val figures = splitFigures(tiles)
    val mappedFigures = toFigures(figures)

    Context set context.copy(gnMelded = mappedFigures)
  }

  @Given("prevalent wind is $wind")
  def givenPrevalentWind(wind: String) {
    Context set context.copy(gnPrevalentWind = WindFamily(wind))
  }

  @Given("player wind is $wind")
  def givenPlayertWind(wind: String) {
    Context set context.copy(gnPlayerWind = WindFamily(wind))
  }

  @Given( """player goes out on "$lastTile" "$lastTileOrigin"""")
  def givenLastTile(lastTile: String, lastTileOrigin: String) {
    val mappedLastTile = Tile(lastTile)
    val mappedLastTileOrigin = TileOrigin(lastTileOrigin)
    val updatedLastTileContext = context.gnLastTileContext.copy(
      tile = mappedLastTile,
      origin = mappedLastTileOrigin
    )
    Context set context.copy(gnLastTileContext = updatedLastTileContext)
  }

  @When("scoring")
  def whenScoring = {

    Result set HuFinder(context.playerTiles, context.playerContext).find
  }


  @Then( """"$combination" is scored""")
  def thenCombination(combination: String) {
    checkHasResult
    assert(result(0).hasCombination(combination),
      combination + " not found in result. Result is" + result)
  }

  @Then( """"$combination" is not scored""")
  def thenNotCombinatin(combination: String) {
    assert(!result(0).hasCombination(combination),
      combination + " found in result. Result is " + result)
  }

  private def checkHasResult {
    assert(!result.isEmpty, "No results")
  }

  /**
   *
   *
   * @param tiles a list of figures. for instance "b2-b3-b4 dr-dr c1 c5"
   * @return a list of tiles. for instance List("b2","b3","b4","dr","dr","c1","c5")
   */
  private def splitTiles(tiles: String) = tiles.split( """(\s|-)""")

  private def splitFigures(tiles: String) = tiles.split( """(\s)""")

  private def toFigures(figures: Seq[String]): Types.Figures = {

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