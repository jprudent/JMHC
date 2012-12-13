package org.liprudent.majiang.mahjong

import org.liprudent.majiang.tiles._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import Tile._
import org.liprudent.majiang.figures._
import org.liprudent.majiang.figures.Dui
import org.liprudent.majiang.tiles.ContextualTile
import org.liprudent.majiang.HuLeFinder

@RunWith(classOf[JUnitRunner])
class MahjongFriendUseCaseSuite extends FunSuite {

  test("case MeldedHand AllChows MixedDoubleChows") {
    val pts = PlayerTiles(Hand(List(b3, b3), ContextualTile(b3, Discarded)), List(Chow(b1, b2, b3), Chow(b5, b6, b7), Chow(c5, c6, c7), Chow(c7, c8, c9)))
    val actual = HuLeFinder(pts).find

    val expected = List(
      DetailedPoints(
        HuLe(List(Dui(b3)),
          List(Chow(b1, b2, b3), Chow(b5, b6, b7), Chow(c5, c6, c7), Chow(c7, c8, c9)),
          ContextualTile(b3, Discarded)),
        List(
          (List(Chow(b1, b2, b3), Chow(b5, b6, b7), Chow(c5, c6, c7), Chow(c7, c8, c9), Dui(b3)), MeldedHand),
          (List(Chow(b1, b2, b3), Chow(b5, b6, b7), Chow(c5, c6, c7), Chow(c7, c8, c9)), AllChows),
          (List(Chow(b5, b6, b7), Chow(c5, c6, c7)), MixedDoubleChows)
        )
      )
    )
    assert(actual === expected)
  }

}
