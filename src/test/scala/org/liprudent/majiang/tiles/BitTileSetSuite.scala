package org.liprudent.majiang.tiles

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.liprudent.majiang.tiles.Tile._
import org.liprudent.majiang.tiles.Types._
import collection.BitSet


@RunWith(classOf[JUnitRunner])
class BitTileSetSuite extends FunSuite {

  test("size") {
   val ts = BitSetTileSet(Integer.parseInt("432101234",8))
   assert(ts.size === 20)
  }


}