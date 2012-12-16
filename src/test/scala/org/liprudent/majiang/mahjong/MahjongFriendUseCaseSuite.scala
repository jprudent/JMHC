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
    val givenContext = PlayerContext(WestWind, EastWind)

    val thenClosed = List(Dui(b3))
    val thenCombinations = List(
      (List(Chow(b1, b2, b3), Chow(b5, b6, b7), Chow(c5, c6, c7), Chow(c7, c8, c9), Dui(b3)), MeldedHand),
      (List(Chow(b1, b2, b3), Chow(b5, b6, b7), Chow(c5, c6, c7), Chow(c7, c8, c9)), AllChows),
      (List(Chow(b5, b6, b7), Chow(c5, c6, c7)), MixedDoubleChows),
      (List(Dui(b3)), SingleWait)
    )

    test(givenClosed, givenDisclosed, givenContextualTile, givenContext, thenClosed, thenCombinations, 10)

  }

  test("case Mixed Triple Chows - All Chows") {
    val givenClosed = TileSet(List(s2, s3, s4, s6, s7, s8, s9, s9))
    val givenContextualTile = ContextualTile(s9, Discarded)
    val givenDisclosed = List(Chow(b6, b7, b8), Chow(c6, c7, c8))
    val givenContext = PlayerContext(WestWind, EastWind)

    val thenClosed: List[Figure with Product] = List(Chow(s2, s3, s4), Chow(s6, s7, s8), Dui(s9))
    val thenCombinations: List[(List[Chow], Combination)] = List(
      (List(Chow(b6, b7, b8), Chow(c6, c7, c8), Chow(s6, s7, s8)), MixedTripleChow),
      (List(Chow(b6, b7, b8), Chow(c6, c7, c8), Chow(s2, s3, s4), Chow(s6, s7, s8)), AllChows)
    )

    test(givenClosed, givenDisclosed, givenContextualTile, givenContext, thenClosed, thenCombinations, 10)
  }



  test("case Upper Four - Flowers - Double Chow - Closed Wait") {

    val givenClosed = TileSet(List(s6, s6, s6, s7, s8, s9, c6, c6))
    val givenDisclosed: List[Chow] = List(Chow(b6, b7, b8), Chow(c7, c8, c9))
    val givenContextualTile: ContextualTile = ContextualTile(s8, Discarded)
    val givenBonus: Bonus = Bonus(List(fb, ss, sa))
    val givenContext = PlayerContext(WestWind, EastWind)

    val thenClosed = List(Pung(s6), Chow(s7, s8, s9), Dui(c6))
    val thenCombinations =
      List(
        (List(Pung(s6), Chow(b6, b7, b8), Chow(c7, c8, c9), Chow(s7, s8, s9), Dui(c6)), UpperFour),
        (List(Chow(c7, c8, c9), Chow(s7, s8, s9)), MixedDoubleChows),
        (List(Chow(s7, s8, s9)), ClosedWait),
        (List(Bonus(List(fb, ss, sa))), FlowerTiles)
      )

    test(givenClosed, givenDisclosed, givenContextualTile, givenBonus, givenContext, thenClosed, thenCombinations, 17)

  }

  test("case All Types - SeatWind - Flower - Single Wait") {

    val givenClosed = TileSet(List(b2, b2, b2, c1, c2, c3, dg, dg))
    val givenDisclosed = List(Pung(ww), Chow(s3, s4, s5))
    val givenContextualTile: ContextualTile = ContextualTile(dg, Discarded)
    val givenBonus: Bonus = Bonus(List(fo))
    val givenContext = PlayerContext(WestWind, EastWind)

    val thenClosed = List(Pung(b2), Chow(c1, c2, c3), Dui(dg))
    val thenCombinations =
      List(
        (List(Pung(b2), Pung(ww), Chow(c1, c2, c3), Chow(s3, s4, s5), Dui(dg)), AllTypes),
        (List(Pung(ww)), SeatWind),
        (List(Dui(dg)), SingleWait),
        (List(Bonus(List(fo))), FlowerTiles)
      )

    test(givenClosed, givenDisclosed, givenContextualTile, givenBonus, givenContext, thenClosed, thenCombinations, 10)

  }

  test("case Knitted Straight - Fully Concealed Hand") {
    val givenClosed = TileSet(List(b1, b4, b7, c2, c5, c8, s3, s6, s9, c8, c8, c8, dr, dr))
    val givenDisclosed = Nil
    val givenContextualTile: ContextualTile = ContextualTile(b1, SelfDrawn)
    val givenBonus: Bonus = Bonus(List(fo))
    val givenContext = PlayerContext(WestWind, EastWind)

    val thenClosed = List(Knitted(Bamboo, Character, Stone), Pung(c8), Dui(dr))
    val thenCombinations =
      List(
        (List(Knitted(Bamboo, Character, Stone)), KnittedStraight),
        (List(Knitted(Bamboo, Character, Stone), Pung(c8), Dui(dr)), FullyConcealedHand),
        (List(Bonus(List(fo))), FlowerTiles)
      )

    test(givenClosed, givenDisclosed, givenContextualTile, givenBonus, givenContext, thenClosed, thenCombinations, 17)

  }

  test("case Knitted Straight - All Types") {

    val givenClosed = TileSet(List(b1, b4, b7, c2, c5, c8, s3, s6, s9, dr, dr))
    val givenDisclosed = List(Pung(ww))
    val givenContextualTile: ContextualTile = ContextualTile(b1, Discarded)
    val givenBonus: Bonus = Bonus(List(fo))
    val givenContext = PlayerContext(WestWind, EastWind)

    val thenClosed = List(Knitted(Bamboo, Character, Stone), Dui(dr))
    val thenCombinations =
      List(
        (List(Knitted(Bamboo, Character, Stone)), KnittedStraight),
        (List(Knitted(Bamboo, Character, Stone), Pung(ww), Dui(dr)), AllTypes),
        (List(Pung(ww)), SeatWind),
        (List(Bonus(List(fo))), FlowerTiles)
      )

    test(givenClosed, givenDisclosed, givenContextualTile, givenBonus, givenContext, thenClosed, thenCombinations, 21)

  }

  test("case Mixed Triple Chow - Fully Concealed Hand, All Chows") {
    val givenClosed = TileSet(List(b1, b2, b3, b7, b8, b9, c7, c8, c9, s7, s8, s9, dr, dr))
    val givenDisclosed = Nil
    val givenContextualTile: ContextualTile = ContextualTile(b1, SelfDrawn)
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(WestWind, EastWind)

    val thenClosed = List(Chow(b1, b2, b3), Chow(b7, b8, b9), Chow(c7, c8, c9), Chow(s7, s8, s9), Dui(dr))
    val thenCombinations =
      List(
        (List(Chow(b7, b8, b9), Chow(c7, c8, c9), Chow(s7, s8, s9)), MixedTripleChow),
        (List(Chow(b1, b2, b3), Chow(b7, b8, b9), Chow(c7, c8, c9), Chow(s7, s8, s9), Dui(dr)), FullyConcealedHand),
        (List(Chow(b1, b2, b3), Chow(b7, b8, b9), Chow(c7, c8, c9), Chow(s7, s8, s9)), AllChows)
      )

    test(givenClosed, givenDisclosed, givenContextualTile, givenBonus, givenContext, thenClosed, thenCombinations, 14)

  }

  private def test(
                    givenClosed: TileSet,
                    givenDisclosed: List[Figure],
                    givenContextualTile: ContextualTile,
                    givenBonus: Bonus,
                    givenContext: PlayerContext,
                    thenClosed: List[Figure],
                    thenCombinations: List[(List[Figure], Combination)],
                    thenTotal: Int
                    ) {

    val pts = PlayerTiles(Hand(givenClosed, givenContextualTile),
      givenDisclosed,
      givenBonus)

    val actual = HuLeFinder(pts, givenContext).find

    val expected = List(
      DetailedPoints(
        HuLe(thenClosed.sorted(OrdFigure),
          givenDisclosed.sorted(OrdFigure),
          givenContextualTile,
          givenContext,
          givenBonus),
        thenCombinations.map {
          case (figures, combination) => (figures.sorted(OrdFigure), combination)
        })
    )

    assert(actual(0).huLe === expected(0).huLe)
    assert(actual === expected)
    assert(actual(0).total === thenTotal)

  }

  private def test(
                    givenClosed: TileSet,
                    givenDisclosed: List[Figure],
                    givenContextualTile: ContextualTile,
                    givenContext: PlayerContext,
                    thenClosed: List[Figure],
                    thenCombinations: List[(List[Figure], Combination)],
                    thenTotal: Int
                    ) {
    test(givenClosed, givenDisclosed, givenContextualTile, Bonus(Nil), givenContext, thenClosed, thenCombinations, thenTotal)
  }

}
