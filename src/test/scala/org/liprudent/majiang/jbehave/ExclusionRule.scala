package org.liprudent.majiang.jbehave

import org.jbehave.core.junit.JUnitStories
import collection.mutable.ListBuffer
import collection.convert.Wrappers.MutableBufferWrapper
import org.jbehave.core.configuration.scala.ScalaContext
import org.jbehave.core.steps.scala.ScalaStepsFactory
import org.jbehave.core.configuration.MostUsefulConfiguration
import org.jbehave.core.reporters.{CrossReference, StoryReporterBuilder}
import org.jbehave.core.io.CodeLocations
import org.jbehave.core.reporters.Format


class ExclusionRule extends AbstractJbehaveBoilerPlate {

  override val storyPaths = MutableBufferWrapper(ListBuffer("org/liprudent/majiang/jbehave/greenbook_scenario"))


}