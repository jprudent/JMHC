package org.liprudent.majiang.greenbook

import org.jbehave.core.steps.Steps
import org.jbehave.core.annotations._
import org.liprudent.majiang.tiles._
import org.liprudent.majiang.mahjong._
import org.liprudent.majiang.HuFinder
import org.liprudent.majiang.mahjong.PlayerTiles
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

class MahjongSteps extends Steps {

  var context: Context = null

  var result: List[DetailedPoints] = null


  @BeforeScenario
  def init() {
    context = Context(
      gnConcealed = Nil,
      thConcealed = Nil,
      gnLastTileContext = ContextualTile(Tile.we, Discarded, NotLastTile),
      gnMelded = Nil,
      gnPlayerWind = EastWind,
      gnPrevalentWind = NorthWind
    )
  }

  @Given( """concealed "$tiles"""")
  def givenConcealed(tiles: String) {
    val splitted = tiles.split( """(\s|-)""")
    println(splitted.toList)
    context = context.copy(gnConcealed = splitted.map {
      tile => Tile(tile)
    }.toList)
  }

  @Given("prevalent wind is $wind")
  def givenPrevalentWind(wind: String) {
    context = context.copy(gnPrevalentWind = WindFamily(wind))
  }

  @Given("player wind is $wind")
  def givenPlayertWind(wind: String) {
    context = context.copy(gnPlayerWind = WindFamily(wind))
  }

  @When("scoring")
  def whenScoring = {

    result = HuFinder(context.playerTiles, context.playerContext).find
  }


  @Then("$combination is scored")
  def thenCombinatin(combination: String) {
    println("then " + combination + " is scored")
    //throw new RuntimeException(combination)
  }

  @Then("$combination is not scored")
  def thenNotCombinatin(combination: String) {
    println("then " + combination + " is NOT scored")
    //throw new RuntimeException(combination)
  }


}