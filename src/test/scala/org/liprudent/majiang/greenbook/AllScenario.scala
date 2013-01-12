package org.liprudent.majiang.greenbook

import org.jbehave.core.junit.JUnitStories
import collection.mutable.ListBuffer
import collection.convert.Wrappers.MutableBufferWrapper
import org.jbehave.core.configuration.scala.ScalaContext
import org.jbehave.core.steps.scala.ScalaStepsFactory
import org.jbehave.core.configuration.MostUsefulConfiguration
import org.jbehave.core.reporters.{CrossReference, StoryReporterBuilder}
import org.jbehave.core.io.CodeLocations
import org.jbehave.core.reporters.Format


class AllScenario extends JUnitStories {

  val xref = new CrossReference();

  configuredEmbedder().embedderControls()

    .doGenerateViewAfterStories(true)
    .doIgnoreFailureInStories(false)
    .doVerboseFailures(true)


  override val stepsFactory = new ScalaStepsFactory(configuration(),
    new ScalaContext("org.liprudent.majiang.greenbook.MahjongSteps")
  )

  override def configuration =
    new MostUsefulConfiguration()
      .useStoryReporterBuilder(
      new StoryReporterBuilder() //
        .withCodeLocation(CodeLocations.codeLocationFromClass(getClass)) //
        .withDefaultFormats() //
        .withFormats(Format.TXT, Format.CONSOLE, Format.HTML_TEMPLATE) //
        .withFailureTrace(true) //
        .withFailureTraceCompression(true) //
        .withCrossReference(xref)
    )
      .useStepMonitor(xref.getStepMonitor)


  override val storyPaths = MutableBufferWrapper(ListBuffer("greenbook_scenario"))


}