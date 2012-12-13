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

  test("ordering") {
    val sut = List(
      Dui(b2),
      Chow(b1, b2, b3),
      Pung(dr),
      Knitted(Bamboo, Stone, Character),
      SomeKnittedWithSomeDragons(List(b1, b4, b7, c2, c5, c8, s3, s6, s9), List(we, wn, ww, ws, dr)),
      ThirteenOrphans(dr)
    )

    val actual = sut.sorted(OrdFigure)

    val expected = List(
      ThirteenOrphans(dr),
      SomeKnittedWithSomeDragons(List(b1, b4, b7, c2, c5, c8, s3, s6, s9), List(we, wn, ww, ws, dr)),
      Knitted(Bamboo, Stone, Character),
      Pung(dr),
      Chow(b1, b2, b3),
      Dui(b2)
    )

    assert(actual === expected)

  }

}
