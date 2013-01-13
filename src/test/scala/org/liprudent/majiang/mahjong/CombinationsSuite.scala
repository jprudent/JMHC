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
      ContextualTile(b3, Discarded, NotLastTile),
      PlayerContext(WestWind, EastWind))
    val actual = AllChows.find(hule)
    val expected = Result(List(Chow(b1, b2, b3), Chow(b5, b6, b7), Chow(c5, c6, c7), Chow(c7, c8, c9)))
    assert(actual === expected)
  }

  test("melded hand") {
    val hule = HuLe(
      List(Dui(b3)),
      List(Chow(b1, b2, b3), Chow(b5, b6, b7), Chow(c5, c6, c7), Chow(c7, c8, c9)),
      ContextualTile(b3, Discarded, NotLastTile),
      PlayerContext(WestWind, EastWind))
    val actual = MeldedHand.find(hule)
    val expected = Result(List(Chow(b1, b2, b3), Chow(b5, b6, b7), Chow(c5, c6, c7), Chow(c7, c8, c9), Dui(b3)))
    assert(actual === expected)
  }

  test("mixed double chows") {
    val hule = HuLe(
      List(Dui(b3)),
      List(Chow(b1, b2, b3), Chow(b5, b6, b7), Chow(c5, c6, c7), Chow(c7, c8, c9)),
      ContextualTile(b3, Discarded, NotLastTile),
      PlayerContext(WestWind, EastWind))
    val actual = MixedDoubleChows.find(hule)
    val expected = Result(List(Chow(b5, b6, b7), Chow(c5, c6, c7)))
    assert(actual === expected)
  }

  test("Three suited terminal chow") {
    val hule = HuLe(
      List(Dui(b5)),
      List(Chow(c1), Chow(c7), Chow(s1), Chow(s7)),
      ContextualTile(b5, Discarded, NotLastTile),
      PlayerContext(WestWind, EastWind))
    val actual = ThreeSuitedTerminalChows.find(hule)
    val expected = Result(List(Chow(c1), Chow(c7), Chow(s1), Chow(s7), Dui(b5)))
    assert(actual === expected)
  }

  test("Three suited terminal invalid, not three family") {
    val hule = HuLe(
      List(Dui(c5)),
      List(Chow(c1), Chow(c7), Chow(s1), Chow(s7)),
      ContextualTile(b5, Discarded, NotLastTile),
      PlayerContext(WestWind, EastWind))
    val actual = ThreeSuitedTerminalChows.find(hule)
    val expected = EmptyResult
    assert(actual === expected)
  }

  test("Three suited terminal invalid, no pair of 5") {
    val hule = HuLe(
      List(Dui(b4)),
      List(Chow(c1), Chow(c7), Chow(s1), Chow(s7)),
      ContextualTile(b5, Discarded, NotLastTile),
      PlayerContext(WestWind, EastWind))
    val actual = ThreeSuitedTerminalChows.find(hule)
    val expected = EmptyResult
    assert(actual === expected)
  }

  test("flower tiles") {

    val hule = HuLe(
      List(Dui(b3)),
      List(Chow(b1, b2, b3), Chow(b5, b6, b7), Chow(c5, c6, c7), Chow(c7, c8, c9)),
      ContextualTile(b3, Discarded, NotLastTile),
      PlayerContext(WestWind, EastWind),
      bonus = Bonus(List(fb, sa))
    )

    val actual = FlowerTiles.find(hule)
    val expected = Result(List(Bonus(List(fb, sa))))

    assert(actual === expected)

  }

  test("closed wait") {
    val disclosed: List[Chow] = List(Chow(b6, b7, b8), Chow(c7, c8, c9))
    val contextualTile: ContextualTile = ContextualTile(s8, Discarded, NotLastTile)
    val context = PlayerContext(WestWind, EastWind)
    val hule = HuLe(List(Pung(s6), Chow(s7, s8, s9), Dui(c6)),
      disclosed,
      contextualTile,
      context)

    val actual = ClosedWait.find(hule)
    val expected = Result(List(Chow(s7, s8, s9)))

    assert(actual === expected)

  }

  test("all types") {
    val closed = List(Pung(b2), Chow(c1, c2, c3), Dui(dg))
    val disclosed = List(Pung(ww), Chow(s3, s4, s5))
    val lastTile: ContextualTile = ContextualTile(dg, Discarded, NotLastTile)
    val context = PlayerContext(WestWind, EastWind)
    val hule = HuLe(closed, disclosed, lastTile, context)

    val actual = AllTypes.find(hule)
    val expected = Result((closed ::: disclosed).sorted(OrdFigure))

    assert(actual === expected)
  }

  test("seat wind") {
    val closed = List(Pung(b2), Chow(c1, c2, c3), Dui(dg))
    val disclosed = List(Pung(ww), Chow(s3, s4, s5))
    val lastTile: ContextualTile = ContextualTile(dg, Discarded, NotLastTile)
    val context = PlayerContext(WestWind, EastWind)
    val hule = HuLe(closed, disclosed, lastTile, context)

    val actual = SeatWind.find(hule)
    val expected = Result(List(Pung(ww)))

    assert(actual === expected)
  }

  test("single wait") {
    val closed = List(Pung(b2), Chow(c1, c2, c3), Dui(dg))
    val disclosed = List(Pung(ww), Chow(s3, s4, s5))
    val lastTile: ContextualTile = ContextualTile(dg, Discarded, NotLastTile)
    val context = PlayerContext(WestWind, EastWind)

    val hule = HuLe(closed, disclosed, lastTile, context)

    val actual = SingleWait.find(hule)
    val expected = Result(List(Dui(dg)))

    assert(actual === expected)
  }

  test("Pung of terminal or honor") {
    val closed: List[Figure] = List(Pung(ww), Chow(c1, c2, c3), Dui(ws))
    val disclosed: List[Figure] = List(Pung(b9), Pung(dr))
    val lastTile: ContextualTile = ContextualTile(ww, Discarded, NotLastTile)
    val context = PlayerContext(WestWind, EastWind)

    val hule = HuLe(closed, disclosed, lastTile, context)

    val actual = PungOfTerminalsOrHonors.find(hule)
    val expected = Result(List(List(Pung(b9)), List(Pung(ww))))

    assert(actual === expected)

  }

  test("Pure shifted chows") {
    val closed: List[Figure] = List(Chow(c1), Chow(c2), Chow(c3), Chow(c4), Dui(c5))
    val disclosed: List[Figure] = List()
    val lastTile: ContextualTile = ContextualTile(c5, Discarded, NotLastTile)
    val context = PlayerContext(WestWind, EastWind)

    val hule = HuLe(closed, disclosed, lastTile, context)

    val actual = PureShiftedChow.find(hule)
    val expected = Result(List(List(Chow(c1), Chow(c2), Chow(c3)), List(Chow(c2), Chow(c3), Chow(c4))))

    assert(actual === expected)

  }

  test("Four concealed pungs but finish with discarded tile so it's 3 pungs") {
    val closed: List[Figure] = List(Pung(b2), Pung(b4), Pung(c6), Pung(s8), Dui(b6))
    val disclosed: List[Figure] = List()
    val lastTile: ContextualTile = ContextualTile(b6, Discarded, NotLastTile)
    val context = PlayerContext(WestWind, EastWind)

    val hule = HuLe(closed, disclosed, lastTile, context)

    val actual = FourConcealedPungs.find(hule)
    val expected = Result(List(List(Pung(b2), Pung(b4), Pung(c6), Pung(s8))))

    assert(actual === expected)

  }

  test("Little Four winds") {
    val closed: List[Figure] = List(Pung(wn), Pung(ww), Pung(ws), Pung(dr), Dui(we))
    val disclosed: List[Figure] = List()
    val lastTile: ContextualTile = ContextualTile(dr, Discarded, NotLastTile)
    val context = PlayerContext(WestWind, EastWind)

    val hule = HuLe(closed, disclosed, lastTile, context)

    val actual = LittleFourWinds.find(hule)
    val expected = Result(List(List(Pung(wn), Pung(ww), Pung(ws), Dui(we))))

    assert(actual === expected)

  }

  test("Mixed Triple Chow not found on PureTripleChow") {
    val closed: List[Figure] = List(Pung(dg), Chow(b2), Chow(b2), Dui(b6))
    val disclosed: List[Figure] = List(Chow(b2))
    val lastTile: ContextualTile = ContextualTile(dg, Discarded, NotLastTile)
    val context = PlayerContext(WestWind, EastWind)

    val hule = HuLe(closed, disclosed, lastTile, context)

    val actual = MixedTripleChows.find(hule)
    val expected = EmptyResult

    assert(actual === expected)

  }

  test("Mixed Triple Chow found") {
    val closed: List[Figure] = List(Pung(dg), Chow(b2), Chow(c2), Dui(b6))
    val disclosed: List[Figure] = List(Chow(s2))
    val lastTile: ContextualTile = ContextualTile(dg, Discarded, NotLastTile)
    val context = PlayerContext(WestWind, EastWind)

    val hule = HuLe(closed, disclosed, lastTile, context)

    val actual = MixedTripleChows.find(hule)
    val expected = Result(List(Chow(b2), Chow(c2), Chow(s2)))

    assert(actual === expected)

  }


}

