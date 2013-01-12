package org.liprudent.majiang.tiles

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import Tile._
import org.liprudent.majiang.figures._

@RunWith(classOf[JUnitRunner])
class FiguresSuite extends FunSuite {

  test("Knitted toTiles") {
    val actual = Knitted(Bamboo, Character, Stone).toTiles
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

  test("ordering Chow") {
    val sut = List(Chow(c5, c6, c7), Chow(b5, b6, b7))
    val actual = sut.sorted(OrdChow)
    val expected = List(Chow(b5, b6, b7), Chow(c5, c6, c7))
    assert(actual === expected)
  }

  test("ordering 2") {
    val sut = List(Pung(ww), Pung(b2), Chow(c1, c2, c3), Chow(s3, s4, s5), Dui(dg))

    val actual = sut.sorted(OrdFigure)
    val expected = actual

    assert(actual === expected)
  }

}
