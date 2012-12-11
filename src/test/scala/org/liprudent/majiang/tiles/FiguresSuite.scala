package org.liprudent.majiang.tiles

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import Tile._
import org.liprudent.majiang.figures._

@RunWith(classOf[JUnitRunner])
class FiguresSuite extends FunSuite {

  test("Knitted asList") {
    val actual = Knitted(Bamboo, Character, Stone).asList
    val expected = List(b1, b4, b7, c2, c5, c8, s3, s6, s9)
    assert(actual.forall(t => expected.contains(t)))
    assert(expected.forall(t => actual.contains(t)))
    assert(actual == expected, actual + "\n" + expected)
  }

}
