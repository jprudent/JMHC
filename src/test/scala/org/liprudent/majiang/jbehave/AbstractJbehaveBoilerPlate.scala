package org.liprudent.majiang.jbehave

import org.jbehave.core.junit.JUnitStories
import org.jbehave.core.reporters.{Format, StoryReporterBuilder, CrossReference}
import org.jbehave.core.steps.scala.ScalaStepsFactory
import org.jbehave.core.configuration.scala.ScalaContext
import org.jbehave.core.configuration.MostUsefulConfiguration
import org.jbehave.core.io.CodeLocations

abstract class AbstractJbehaveBoilerPlate extends JUnitStories {

  val xref = new CrossReference();

  configuredEmbedder().embedderControls()

    .doGenerateViewAfterStories(true)
    .doIgnoreFailureInStories(false)
    .doVerboseFailures(true)


  override val stepsFactory = new ScalaStepsFactory(configuration(),
    new ScalaContext("org.liprudent.majiang.jbehave.MahjongSteps")
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


}
