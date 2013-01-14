package org.liprudent.majiang.tiles

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.liprudent.majiang.tiles.Tile._
import org.liprudent.majiang.tiles.Types._


@RunWith(classOf[JUnitRunner])
class TileSetSuite extends FunSuite {

  test("remove when multiple") {
    val hand = TileSet(List(b1, b2, b3, b1, b2, b2, b2))
    val actual = hand removed (b1)
    val expected = TileSet(List(/*X*/ b2, b3, b1, b2, b2, b2))
    assert(actual === expected)
  }

  test("remove when single") {
    val hand = TileSet(List(b1, b2, b3, b1, b2, b2, b2))
    val actual = hand removed (b3)
    val expected = TileSet(List(b1, b2, /*X*/ b1, b2, b2, b2))
    assert(actual === expected)
  }

  test("remove several") {
    val hand = TileSet(List(b1, b2, b2, b2, b2, b3))
    val actual = hand removed (List(b1, b3))
    val expected = TileSet(List(b2, b2, b2, b2))
    assert(actual === expected)
  }

  test("Split by family") {
    val hand = TileSet(List(b2, c1, b1, s1, c2, dr, dr, we, ww))

    val families: List[List[TileOccurence]] = hand.splitByFamily
    assert(families === List(
      List((b1, 1), (b2, 1)),
      List((c1, 1), (c2, 1)),
      List((s1, 1)),
      List((we, 1)),
      List((ww, 1)),
      List((dr, 2))))

  }
}