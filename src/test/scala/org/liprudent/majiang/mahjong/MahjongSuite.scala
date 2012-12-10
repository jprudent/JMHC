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
    val allChows_mixedTripleChow = PlayerTiles(Hand(List(b9, b7, b8, c7, c8, c9, s7, s8, s9, b1, b2, b3, dr, dr)), Nil)
  }

  test("a mahjong has 14 tiles") {
    new Hands {
      assert(MahjongFinder(invalid).quickValid == false)
      assert(MahjongFinder(valid).quickValid == true)
      assert(MahjongFinder(noMahjong).quickValid == true)
    }
  }

  ignore("an invalid hand has no mahjong") {
    new Hands {
      val actual = MahjongFinder(invalid).find
      assert(actual == Nil, actual)
    }
  }

  ignore("if there is no mahjong ... well there is no mahjong") {
    new Hands {
      val actual = MahjongFinder(noMahjong).find
      assert(actual == Nil, actual)
    }
  }

  test("if there is a mahjong ... well there is a mahjong") {
    new Hands {
      val actual = MahjongFinder(allChows_mixedTripleChow).find
      val expected = List(
        (List(Chow(b6, b7, b8), Chow(c6, c7, c8), Chow(s6, s7, s8)), MixedTripleChow),
        (List(Chow(b6, b7, b8), Chow(c6, c7, c8), Chow(s2, s3, s4), Chow(s6, s7, s8)), AllChows)
      )
      assert(actual == expected, actual)
    }
  }

  test("isWellFormedMahjong") {
    new Hands {
      val validFigures = List(Chow(b6, b7, b8), Chow(c6, c7, c8), Chow(s2, s3, s4), Chow(s6, s7, s8), Dui(dr))
      assert(MahjongFinder.isWellFormedMahjong(validFigures, Nil))
    }
  }

  test("find figures when there is no figures") {
    new Hands {
      val expected = Set()
      val actual = MahjongFinder.findFigures(Hand(List[Tile]()))
      assert(actual == expected, actual)
    }
  }

  test("find figures when there is one") {
    val expected = Set(List(Chow(b1, b2, b3)))
    val actual = MahjongFinder.findFigures(Hand(List(b2, b3, b1)))
    assert(actual == expected, actual)
  }

  test("find figures when there are two") {
    val expected = List(
      List(Chow(b1, b2, b3), Chow(b1, b2, b3), Chow(b1, b2, b3)),
      List(Pung(b1), Pung(b2), Pung(b3)))

    val actual = MahjongFinder.findFigures(Hand(List(b1, b2, b3, b1, b2, b3, b1, b2, b3)))

    def nicePrint(lstFigures: Set[Figures]) {
      lstFigures.foreach {
        figures => println("Solution : ")
        figures.foreach(figure => println("\t" + figure))
      }
    }

    nicePrint(actual)

    assert(expected.forall(exp => actual.contains(exp)), actual)
  }

  test("s2s3s4s6s7s8s9s9 c6c7c8b6b7b8") {
    val expected = List(
      (List(Chow(b6, b7, b8), Chow(c6, c7, c8), Chow(s2, s3, s4), Chow(s6, s7, s8)), AllChows),
      (List(Chow(b6, b7, b8), Chow(c6, c7, c8), Chow(s6, s7, s8)), MixedTripleChow)
    )

    val actual = Points(
      HuLe(
        List(Chow(s2, s3, s4), Chow(s6, s7, s8), Dui(s9)),
        List(Chow(b6, b7, b8), Chow(c6, c7, c8))
      )
    )

    assert(actual == expected, actual)
  }
}
