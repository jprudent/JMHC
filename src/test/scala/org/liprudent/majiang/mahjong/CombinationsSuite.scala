package org.liprudent.majiang.mahjong

import org.liprudent.majiang.tiles._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import Tile._
import org.liprudent.majiang.figures._
import org.liprudent.majiang.figures.Dui
import org.liprudent.majiang.tiles.ContextualTile

@RunWith(classOf[JUnitRunner])
class CombinationsSuite extends FunSuite {
  test("All Chows") {
    val hule = HuLe(
      List(Dui(b3)),
      List(Chow(b1, b2, b3), Chow(b5, b6, b7), Chow(c5, c6, c7), Chow(c7, c8, c9)),
      ContextualTile(b3, Discarded))
    val actual = AllChows.find(hule)
    val expected = Some(List(Chow(b1, b2, b3), Chow(b5, b6, b7), Chow(c5, c6, c7), Chow(c7, c8, c9)))
    assert(actual === expected)
  }

  test("melded hand") {
    val hule = HuLe(
      List(Dui(b3)),
      List(Chow(b1, b2, b3), Chow(b5, b6, b7), Chow(c5, c6, c7), Chow(c7, c8, c9)),
      ContextualTile(b3, Discarded))
    val actual = MeldedHand.find(hule)
    val expected = Some(List(Chow(b1, b2, b3), Chow(b5, b6, b7), Chow(c5, c6, c7), Chow(c7, c8, c9), Dui(b3)))
    assert(actual === expected)
  }

  test("mixed double chows") {
    val hule = HuLe(
      List(Dui(b3)),
      List(Chow(b1, b2, b3), Chow(b5, b6, b7), Chow(c5, c6, c7), Chow(c7, c8, c9)),
      ContextualTile(b3, Discarded))
    val actual = MixedDoubleChows.find(hule)
    val expected = Some(List(Chow(b5, b6, b7), Chow(c5, c6, c7)))
    assert(actual === expected)
  }
}

