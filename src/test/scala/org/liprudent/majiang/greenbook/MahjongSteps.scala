package org.liprudent.majiang.greenbook

import collection.mutable.ListBuffer
import org.jbehave.core.steps.Steps
import org.jbehave.core.annotations._

class MahjongSteps extends Steps {
  var rolls: ListBuffer[Int] = _

  @BeforeScenario
  def init() {
    println("init scenario")
  }


  @Given("player wind is $wind")
  def givenPlayerWind(wind: String) {
    println("given player wind is " + wind)
    //throw new RuntimeException(wind)
  }

  @When("scoring")
  def whenScoring = {
    println("when scoring")
    //throw new RuntimeException("scoring")
  }


  @Then("$combination is scored")
  def thenCombinatin(combination: String) {
    println("then " + combination + " is scored")
    //throw new RuntimeException(combination)
  }


}