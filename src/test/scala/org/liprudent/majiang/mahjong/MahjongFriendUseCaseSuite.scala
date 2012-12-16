package org.liprudent.majiang.mahjong

import org.liprudent.majiang.tiles._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import Tile._
import org.liprudent.majiang.figures._
import org.liprudent.majiang.HuLeFinder
import org.liprudent.majiang.figures.Pung
import org.liprudent.majiang.figures.Bonus
import org.liprudent.majiang.figures.Dui
import org.liprudent.majiang.tiles.ContextualTile

@RunWith(classOf[JUnitRunner])
class MahjongFriendUseCaseSuite extends FunSuite {

  test("case MeldedHand AllChows MixedDoubleChows") {
    val givenClosed = TileSet(List(b3, b3))
    val givenDisclosed = List(Chow(b1, b2, b3), Chow(b5, b6, b7), Chow(c5, c6, c7), Chow(c7, c8, c9))
    val givenContextualTile = ContextualTile(b3, Discarded)

    val thenClosed = List(Dui(b3))
    val thenCombinations = List(
      (List(Chow(b1, b2, b3), Chow(b5, b6, b7), Chow(c5, c6, c7), Chow(c7, c8, c9), Dui(b3)), MeldedHand),
      (List(Chow(b1, b2, b3), Chow(b5, b6, b7), Chow(c5, c6, c7), Chow(c7, c8, c9)), AllChows),
      (List(Chow(b5, b6, b7), Chow(c5, c6, c7)), MixedDoubleChows)
    )

    test(givenClosed, givenDisclosed, givenContextualTile, thenClosed, thenCombinations, 9)

  }

  test("case Mixed Triple Chows - All Chows") {
    val givenClosed = TileSet(List(s2, s3, s4, s6, s7, s8, s9, s9))
    val givenContextualTile = ContextualTile(s9, Discarded)
    val givenDisclosed = List(Chow(b6, b7, b8), Chow(c6, c7, c8))

    val thenClosed: List[Figure with Product] = List(Chow(s2, s3, s4), Chow(s6, s7, s8), Dui(s9))
    val thenCombinations: List[(List[Chow], Combination)] = List(
      (List(Chow(b6, b7, b8), Chow(c6, c7, c8), Chow(s6, s7, s8)), MixedTripleChow),
      (List(Chow(b6, b7, b8), Chow(c6, c7, c8), Chow(s2, s3, s4), Chow(s6, s7, s8)), AllChows)
    )

    test(givenClosed, givenDisclosed, givenContextualTile, thenClosed, thenCombinations, 10)
  }



  test("case Upper Four - Flowers - Double Chow - Closed Wait") {

    val givenClosed = TileSet(List(s6, s6, s6, s7, s8, s9, c6, c6))
    val givenDisclosed: List[Chow] = List(Chow(b6, b7, b8), Chow(c7, c8, c9))
    val givenContextualTile: ContextualTile = ContextualTile(s8, Discarded)
    val givenBonus: Bonus = Bonus(List(fb, ss, sa))

    val thenClosed = List(Pung(s6), Chow(s7, s8, s9), Dui(c6))
    val thenCombinations =
      List(
        (List(Pung(s6), Chow(b6, b7, b8), Chow(c7, c8, c9), Chow(s7, s8, s9), Dui(c6)), UpperFour),
        (List(Chow(c7, c8, c9), Chow(s7, s8, s9)), MixedDoubleChows),
        (List(Chow(s7, s8, s9)), ClosedWait),
        (List(Bonus(List(fb, ss, sa))), FlowerTiles)
      )

    test(givenClosed, givenDisclosed, givenContextualTile, givenBonus, thenClosed, thenCombinations, 17)

  }

  private def test(
                    givenClosed: TileSet,
                    givenDisclosed: List[Figure],
                    givenContextualTile: ContextualTile,
                    givenBonus: Bonus,
                    thenClosed: List[Figure],
                    thenCombinations: List[(List[Figure], Combination)],
                    thenTotal: Int
                    ) {

    val pts = PlayerTiles(Hand(givenClosed, givenContextualTile),
      givenDisclosed,
      givenBonus)

    val actual = HuLeFinder(pts).find

    val expected = List(
      DetailedPoints(
        HuLe(thenClosed,
          givenDisclosed,
          givenContextualTile,
          givenBonus),
        thenCombinations)
    )

    assert(actual(0).huLe === expected(0).huLe)
    assert(actual === expected)
    assert(actual(0).total === thenTotal)

  }

  private def test(
                    givenClosed: TileSet,
                    givenDisclosed: List[Figure],
                    givenContextualTile: ContextualTile,
                    thenClosed: List[Figure],
                    thenCombinations: List[(List[Figure], Combination)],
                    thenTotal: Int
                    ) {
    test(givenClosed, givenDisclosed, givenContextualTile, Bonus(Nil), thenClosed, thenCombinations, thenTotal)
  }

}
