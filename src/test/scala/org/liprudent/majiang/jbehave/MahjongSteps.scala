package org.liprudent.majiang.jbehave

import org.jbehave.core.steps.Steps
import org.jbehave.core.annotations._
import org.liprudent.majiang.tiles._
import org.liprudent.majiang.HuFinder
import org.liprudent.majiang.mahjong.{Combination, DetailedPoints}
import org.liprudent.majiang.figures.Kong
import org.liprudent.majiang.tiles.ContextualTile
import org.liprudent.majiang.ui.StringMapper

/**
 * Hold all the context for testing
 * This is where all the given goes.
 *
 * @param gnConcealed
 * @param thConcealed
 * @param gnLastTileContext
 * @param gnMelded
 * @param gnPrevalentWind
 * @param gnPlayerWind
 */
case class Context(
                    gnConcealed: List[Tile],
                    gnConcealedKongs: List[Kong],
                    thConcealed: Types.Figures,
                    gnLastTileContext: ContextualTile,
                    gnMelded: Types.Figures,
                    gnPrevalentWind: WindFamily,
                    gnPlayerWind: WindFamily) {
  lazy val concealedTiles = ConcealedTiles(gnConcealed, gnLastTileContext)
  lazy val playerTiles = PlayerTiles(concealedTiles, gnMelded, gnConcealedKongs)
  lazy val playerContext = PlayerContext(gnPlayerWind, gnPrevalentWind)
}

/**
 * A utility to manipulate the context
 */
object Context {

  val default = Context(
    gnConcealed = Nil,
    gnConcealedKongs = Nil,
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

/**
 * A utility to manipulate the result
 */
object Result {
  val instance = new ThreadLocal[List[DetailedPoints]]

  def get = instance.get()

  def set(res: List[DetailedPoints]) {
    instance.set(res)
  }
}

/**
 * A utility to track what has been checked by @Then annotations
 */
object ThenTracking {

  val instance = new ThreadLocal[List[Combination]]

  def get = instance.get

  def tracked(combination: Combination) =
    set(combination :: get)

  def init = set(Nil)

  private def set(tracked: List[Combination]) =
    instance.set(tracked)

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

  /**
   * Handy method to track what has been checked
   * @return
   */
  def track(combination: Combination) {
    ThenTracking.tracked(combination)
  }


  @BeforeScenario
  def init() {
    Context.init
    ThenTracking.init
  }

  @Given( """concealed "$tiles"""")
  def givenConcealed(tiles: String) {
    val figures = StringMapper.splitFigures(tiles)
    val mappedFigures = StringMapper.toFigures(figures)

    val (givenConcealedKongs, thenConcealed) = mappedFigures.partition(_.isInstanceOf[Kong])

    Context set context.copy(
      gnConcealed = thenConcealed.map(_.toTiles).flatten,
      gnConcealedKongs = givenConcealedKongs.asInstanceOf[List[Kong]],
      thConcealed = thenConcealed)

  }

  @Given( """melded "$tiles"""")
  def givenMelded(tiles: String) {
    val figures = StringMapper.splitFigures(tiles)
    val mappedFigures = StringMapper.toFigures(figures)

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


  @Then( """"$combinationName" is scored""")
  def thenCombination(combinationName: String) {
    checkHasResult
    val combination = result(0).toCombination(combinationName)
    track(combination)
    assert(result(0).hasCombination(combination),
      combinationName + " not found in result. Result is" + result)
  }

  @Then( """"$combinationName" is scored only once""")
  @Alias( """"$combinationName" is scored once""")
  def thenCombinationOnce(combinationName: String) {
    checkHasResult
    val combination = result(0).toCombination(combinationName)
    track(combination)
    assert(result(0).hasCombinationOnce(combination),
      combinationName + " not found in result. Result is" + result)
  }

  @Then( """"$combinationName" is scored twice""")
  def thenCombinationTwice(combinationName: String) {
    checkHasResult
    val combination = result(0).toCombination(combinationName)
    track(combination)
    assert(result(0).hasCombinationTwice(combination),
      combinationName + " not found in result. Result is" + result)
  }

  @Then( """"$combination" is not scored""")
  def thenNotCombinatin(combination: String) {
    assert(!result(0).hasCombination(combination),
      combination + " found in result. Result is " + result)
  }

  @Then("nothing else is scored")
  def thenNothingElseScored {
    val notChecked = result(0).detailedPoints.map(_._2).filterNot(ThenTracking.get.contains(_))
    assert(notChecked == Nil, "Some combinations you didn't explicitly checked has been found : " + notChecked)
  }

  private def checkHasResult {
    assert(!result.isEmpty, "No results. Player tiles are " + context.playerTiles)
  }


}