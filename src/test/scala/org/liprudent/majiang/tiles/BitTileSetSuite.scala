package org.liprudent.majiang.tiles

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.liprudent.majiang.tiles.Tile._


@RunWith(classOf[JUnitRunner])
class BitTileSetSuite extends FunSuite {

  test("size") {
    val ts = BitTileSet(Integer.parseInt("432101234", 8))
    assert(ts.size === 20)
  }

  test("removed when occurrence is 1") {
    val ts = BitTileSet(Integer.parseInt("432101234", 8))

    val expectedRemovedB4 = BitTileSet(Integer.parseInt("432001234", 8))
    val actualRemovedB4 = ts.removed(b4)
    assert(actualRemovedB4 === expectedRemovedB4)

  }

  test("add when occurrence is 3") {
    val ts = BitTileSet(Integer.parseInt("432101234", 8))

    val expectedAddedB2 = BitTileSet(Integer.parseInt("442101234", 8))
    val actualAddedB2 = ts.added(b2)
    assert(actualAddedB2 === expectedAddedB2)

  }

}