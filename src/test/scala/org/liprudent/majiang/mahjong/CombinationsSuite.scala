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
      ContextualTile(b3, Discarded),
      PlayerContext(WestWind, EastWind))
    val actual = AllChows.find(hule)
    val expected = Some(List(Chow(b1, b2, b3), Chow(b5, b6, b7), Chow(c5, c6, c7), Chow(c7, c8, c9)))
    assert(actual === expected)
  }

  test("melded hand") {
    val hule = HuLe(
      List(Dui(b3)),
      List(Chow(b1, b2, b3), Chow(b5, b6, b7), Chow(c5, c6, c7), Chow(c7, c8, c9)),
      ContextualTile(b3, Discarded),
      PlayerContext(WestWind, EastWind))
    val actual = MeldedHand.find(hule)
    val expected = Some(List(Chow(b1, b2, b3), Chow(b5, b6, b7), Chow(c5, c6, c7), Chow(c7, c8, c9), Dui(b3)))
    assert(actual === expected)
  }

  test("mixed double chows") {
    val hule = HuLe(
      List(Dui(b3)),
      List(Chow(b1, b2, b3), Chow(b5, b6, b7), Chow(c5, c6, c7), Chow(c7, c8, c9)),
      ContextualTile(b3, Discarded),
      PlayerContext(WestWind, EastWind))
    val actual = MixedDoubleChows.find(hule)
    val expected = Some(List(Chow(b5, b6, b7), Chow(c5, c6, c7)))
    assert(actual === expected)
  }

  test("flower tiles") {

    val hule = HuLe(
      List(Dui(b3)),
      List(Chow(b1, b2, b3), Chow(b5, b6, b7), Chow(c5, c6, c7), Chow(c7, c8, c9)),
      ContextualTile(b3, Discarded),
      PlayerContext(WestWind, EastWind),
      Bonus(List(fb, sa))
    )

    val actual = FlowerTiles.find(hule)
    val expected = Some(List(Bonus(List(fb, sa))))

    assert(actual === expected)

  }

  test("closed wait") {
    val disclosed: List[Chow] = List(Chow(b6, b7, b8), Chow(c7, c8, c9))
    val contextualTile: ContextualTile = ContextualTile(s8, Discarded)
    val context = PlayerContext(WestWind, EastWind)
    val hule = HuLe(List(Pung(s6), Chow(s7, s8, s9), Dui(c6)),
      disclosed,
      contextualTile,
      context)

    val actual = ClosedWait.find(hule)
    val expected = Some(List(Chow(s7, s8, s9)))

    assert(actual === expected)

  }

  test("all types") {
    val closed = List(Pung(b2), Chow(c1, c2, c3), Dui(dg))
    val disclosed = List(Pung(ww), Chow(s3, s4, s5))
    val lastTile: ContextualTile = ContextualTile(dg, Discarded)
    val context = PlayerContext(WestWind, EastWind)
    val hule = HuLe(closed, disclosed, lastTile, context)

    val actual = AllTypes.find(hule)
    val expected = Some((closed ::: disclosed).sorted(OrdFigure))

    assert(actual === expected)
  }

  test("seat wind") {
    val closed = List(Pung(b2), Chow(c1, c2, c3), Dui(dg))
    val disclosed = List(Pung(ww), Chow(s3, s4, s5))
    val lastTile: ContextualTile = ContextualTile(dg, Discarded)
    val context = PlayerContext(WestWind, EastWind)
    val hule = HuLe(closed, disclosed, lastTile, context)

    val actual = SeatWind.find(hule)
    val expected = Some(List(Pung(ww)))

    assert(actual === expected)
  }

  test("single wait") {
    val closed = List(Pung(b2), Chow(c1, c2, c3), Dui(dg))
    val disclosed = List(Pung(ww), Chow(s3, s4, s5))
    val lastTile: ContextualTile = ContextualTile(dg, Discarded)
    val context = PlayerContext(WestWind, EastWind)

    val hule = HuLe(closed, disclosed, lastTile, context)

    val actual = SingleWait.find(hule)
    val expected = Some(List(Dui(dg)))

    assert(actual === expected)
  }

  test("Pung of terminal or honor") {
    val closed: List[Figure] = List(Pung(ww), Chow(c1, c2, c3), Dui(ws))
    val disclosed: List[Figure] = List(Pung(b9), Pung(dr))
    val lastTile: ContextualTile = ContextualTile(ww, Discarded)
    val context = PlayerContext(WestWind, EastWind)

    val hule = HuLe(closed, disclosed, lastTile, context)

    val actual = PungOfTerminalOrHonors.find(hule)
    val expected = Some(List(Pung(b9)))

    assert(actual === expected)

  }


}

