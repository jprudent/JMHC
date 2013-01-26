package org.liprudent.majiang.jbehave

import collection.mutable.ListBuffer
import collection.convert.Wrappers.MutableBufferWrapper


class AllScenario extends AbstractJbehaveBoilerPlate {


  override val storyPaths = MutableBufferWrapper(ListBuffer("org/liprudent/majiang/jbehave/greenbook_scenario"))


}