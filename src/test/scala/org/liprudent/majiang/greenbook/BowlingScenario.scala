package org.liprudent.majiang.greenbook

import org.jbehave.core.junit.JUnitStories
import collection.mutable.ListBuffer
import collection.convert.Wrappers.MutableBufferWrapper
import org.jbehave.core.configuration.scala.ScalaContext
import org.jbehave.core.steps.scala.ScalaStepsFactory
import org.jbehave.core.configuration.MostUsefulConfiguration
import org.jbehave.core.reporters.StoryReporterBuilder
import org.jbehave.core.io.CodeLocations
import org.jbehave.core.reporters.StoryReporterBuilder.Format


class BowlingScenario extends JUnitStories {

  configuredEmbedder().embedderControls()

    .doGenerateViewAfterStories(true)
    .doIgnoreFailureInStories(false)
    .doVerboseFailures(true)
    .doSkip(false)

  override val stepsFactory = new ScalaStepsFactory(configuration(),
    new ScalaContext("org.liprudent.majiang.greenbook.BowlingSteps")
  )

  override def configuration = new MostUsefulConfiguration()
    .useStoryReporterBuilder(
    new StoryReporterBuilder() //
      .withCodeLocation(CodeLocations.codeLocationFromClass(getClass)) //
      .withDefaultFormats() //
      .withFormats(Format.CONSOLE, Format.HTML) //
      .withFailureTrace(true) //
      .withFailureTraceCompression(true) //
  )


  override val storyPaths = MutableBufferWrapper(ListBuffer("greenbook_scenario"))


}