package org.liprudent.majiang

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class HuLePointsComputerJsonAdapterSuite extends FunSuite {

  test("Given a mahjong, we can compute points") {

    val points = HuLePointsComputerJsonAdapter.wrapOrError(
      "{\"concealed\":\"b1-b2-b3 b1-b2-b3 b5-b5\",\"concealed_kong\":\"b4-b4-b4-b4\",\"melded\":\"b1-b2-b3\",\"winning_tile\":\"b1\",\"winning_tile_origin\":\"SELF DRAWN\",\"last_tile_situation\":\"NOT LAST TILE\",\"prevalent_wind\":\"EAST\",\"player_wind\":\"WEST\"}")
    println(points)
    assert(points.contains("total"))
  }

}
