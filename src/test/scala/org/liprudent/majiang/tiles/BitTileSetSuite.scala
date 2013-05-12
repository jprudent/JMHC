package org.liprudent.majiang.tiles

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.liprudent.majiang.tiles.Tile._


@RunWith(classOf[JUnitRunner])
class BitTileSetSuite extends FunSuite {

  test("size") {
    val ts = BitSetTileSet(Integer.parseInt("432101234", 8))
    assert(ts.size === 20)
  }

  test("removed when occurrence is 1") {
    val ts = BitSetTileSet(Integer.parseInt("432101234", 8))

    val expectedRemovedB4 = BitSetTileSet(Integer.parseInt("432001234", 8))
    val actualRemovedB4 = ts.removed(b4)
    assert(actualRemovedB4 === expectedRemovedB4)


  }

  test("removed when occurrence is 0") {
    val ts = BitSetTileSet(Integer.parseInt("432101234", 8))

    val expectedRemovedB5 = BitSetTileSet(Integer.parseInt("432101234", 8))
    val actualRemovedB5 = ts.removed(b5)
    assert(actualRemovedB5 === expectedRemovedB5)


  }

}