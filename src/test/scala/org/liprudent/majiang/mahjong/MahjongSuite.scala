package org.liprudent.majiang.mahjong

import org.liprudent.majiang.tiles._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import Tile._
import org.liprudent.majiang.figures.{Pung, Dui, Chow}

@RunWith(classOf[JUnitRunner])
class MahjongSuite extends FunSuite {

  trait Hands {
    val valid = PlayerTiles(Hand(List(b1, b2, b3, c4, c5, c6, s7, s8, s9, ww, ww, ww, dr, dr)), Nil)
    val noMahjong = PlayerTiles(Hand(List(b1, b2, b4, c4, c5, c6, s7, s8, s9, ww, ww, ww, dr, dr)), Nil)
    val invalid = PlayerTiles(valid.hand.remove(b1), valid.disclosed)
  }

  test("a mahjong has 14 tiles") {
    new Hands {
      assert(MahjongFinder(invalid).quickValid == false)
      assert(MahjongFinder(valid).quickValid == true)
      assert(MahjongFinder(noMahjong).quickValid == true)
    }
  }

  test("an invalid hand has no mahjong") {
    new Hands {
      assert(MahjongFinder(invalid) == Nil)
    }
  }

  test("if there is no mahjong ... well there is no mahjong") {
    new Hands {
      assert(MahjongFinder(noMahjong) == Nil)
    }
  }

  test("if there is a mahjong ... well there is a mahjong") {
    new Hands {
      assert(MahjongFinder(valid) == Mahjong(
        List(Chow(b1, b2, b3), Chow(c4, c5, c6), Chow(s7, s8, s9), Pung(ww), Dui(dr)),
        Nil
      ))
    }
  }

  test("s2s3s4s6s7s8s9s9 c6c7c8b6b7b8") {
    val expected = List(
      (List(Chow(b6, b7, b8), Chow(c6, c7, c8), Chow(s2, s3, s4), Chow(s6, s7, s8)), AllChows),
      (List(Chow(b6, b7, b8), Chow(c6, c7, c8), Chow(s6, s7, s8)), MixedTripleChow)
    )

    val actual = Points(
      Mahjong(
        List(Chow(s2, s3, s4), Chow(s6, s7, s8), Dui(s9)),
        List(Chow(b6, b7, b8), Chow(c6, c7, c8))
      )
    )

    assert(actual == expected, actual)
  }
}
