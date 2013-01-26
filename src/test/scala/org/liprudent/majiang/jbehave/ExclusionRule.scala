package org.liprudent.majiang.jbehave

import collection.mutable.ListBuffer
import collection.convert.Wrappers.MutableBufferWrapper


class ExclusionRule extends AbstractJbehaveBoilerPlate {

  override val storyPaths = MutableBufferWrapper(ListBuffer("org/liprudent/majiang/jbehave/exclusion_scenario"))


}