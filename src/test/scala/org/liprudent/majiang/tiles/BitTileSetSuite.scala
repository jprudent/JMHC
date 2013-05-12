package org.liprudent.majiang.tiles

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.liprudent.majiang.tiles.Tile._


@RunWith(classOf[JUnitRunner])
class BitTileSetSuite extends FunSuite {

  test("size") {
    val ts = BitTileSet(bamboos = Integer.parseInt("432101234", 8))
    assert(ts.size === 20)
  }

  test("removed when occurrence is 1") {
    val ts = BitTileSet(characters = Integer.parseInt("432101234", 8))

    val expectedRemoved = BitTileSet(characters = Integer.parseInt("432001234", 8))
    val actualRemoved = ts.removed(c4)
    assert(actualRemoved === expectedRemoved)

  }

  test("add when occurrence is 3") {
    val ts = BitTileSet(stones = Integer.parseInt("432101234", 8))

    val expectedAdded = BitTileSet(stones = Integer.parseInt("442101234", 8))
    val actualAdded = ts.added(s2)
    assert(actualAdded === expectedAdded)

  }

  test("is allUnique") {
    val notUnique = BitTileSet(honors = Integer.parseInt("432101234", 8))
    assert(!notUnique.isAllUnique)

    val unique = BitTileSet(honors = Integer.parseInt("0101010", 8))
    assert(unique.isAllUnique)
  }

}