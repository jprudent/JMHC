package org.liprudent.majiang.greenbook

import collection.mutable.ListBuffer
import org.jbehave.core.steps.Steps
import org.jbehave.core.annotations._

class BowlingSteps extends Steps {
  var rolls: ListBuffer[Int] = _

  @BeforeScenario
  def init() {
  }


  @Given("player wind is $wind")
  def givenPlayerWind(wind: String) {
    // throw new RuntimeException(wind)
  }

  @When("scoring")
  def whenScoring = {
    //throw new RuntimeException("scoring")
  }


  @Then("$combination is scored")
  def thenCombinatin(combination: String) {
    //throw new RuntimeException(combination)
  }


}