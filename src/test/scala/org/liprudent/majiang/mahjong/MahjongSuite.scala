package org.liprudent.majiang.mahjong

import org.liprudent.majiang.tiles._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import Tile._
import org.liprudent.majiang.figures.{Knitted, Pung, Dui, Chow}

@RunWith(classOf[JUnitRunner])
class MahjongSuite extends FunSuite {

  trait Hands {
    val valid = PlayerTiles(Hand(List(b1, b2, b3, c4, c5, c6, s7, s8, s9, ww, ww, ww, dr, dr), ContextualTile(b1, SelfDrawn)), Nil)
    val noMahjong = PlayerTiles(Hand(List(b1, b2, b4, c4, c5, c6, s7, s8, s9, ww, ww, ww, dr, dr), ContextualTile(b1, SelfDrawn)), Nil)
    val invalid = PlayerTiles(Hand(List(b2), ContextualTile(b2, SelfDrawn)), valid.disclosed)
    val allChows_mixedTripleChow = PlayerTiles(Hand(List(b9, b7, b8, c7, c8, c9, s7, s8, s9, b1, b2, b3, dr, dr), ContextualTile(b1, SelfDrawn)), Nil)
    val knittedStraight = PlayerTiles(
      Hand(List(b1, b4, b7, c2, c5, c8, s3, s6, s9), ContextualTile(b1, SelfDrawn)),
      List(Pung(c8), Dui(ww))
    )
  }

  test("a mahjong has 14 tiles") {
    new Hands {
      assert(HuLeFinder(invalid).quickValid == false)
      assert(HuLeFinder(valid).quickValid == true)
      assert(HuLeFinder(noMahjong).quickValid == true)
    }
  }

  test("an invalid hand has no mahjong") {
    new Hands {
      val actual = HuLeFinder(invalid).find
      assert(actual == Nil, actual)
    }
  }

  test("if there is no mahjong ... well there is no mahjong") {
    new Hands {
      val actual = HuLeFinder(noMahjong).find
      assert(actual == Nil, actual)
    }
  }

  test("HuLe Finder : b1b2b3b7b8b9c7c8c9s7s8s9drdr") {
    new Hands {
      val actual = HuLeFinder(allChows_mixedTripleChow).find
      val expected = List(
        DetailedPoints(
          HuLe(List(Chow(b1, b2, b3), Chow(b7, b8, b9), Chow(c7, c8, c9), Chow(s7, s8, s9), Dui(dr)), Nil, ContextualTile(b1, SelfDrawn)),
          List(
            (List(Chow(b7, b8, b9), Chow(c7, c8, c9), Chow(s7, s8, s9)), MixedTripleChow),
            (List(Chow(b1, b2, b3), Chow(b7, b8, b9), Chow(c7, c8, c9), Chow(s7, s8, s9)), AllChows)
          )
        )
      )
      assert(actual == expected, actual)
    }
  }

  test("HuLe Finder knitted straight: b1b4b7s2s5s8c3c6c9 c8c8c8wwww") {
    new Hands {
      val actual = HuLeFinder(knittedStraight).find
      val expected = List(
        DetailedPoints(
          HuLe(List(Knitted(Bamboo, Character, Stone)), List(Pung(c8), Dui(ww)), ContextualTile(b1, SelfDrawn)),
          List(
            (List(Knitted(Bamboo, Character, Stone)), KnittedStraight)
          )
        )
      )
      assert(actual === expected)
    }
  }

  test("isWellFormedMahjong") {
    new Hands {
      val validFigures = List(Chow(b6, b7, b8), Chow(c6, c7, c8), Chow(s2, s3, s4), Chow(s6, s7, s8), Dui(dr))
      assert(HuLeFinder.isWellFormedMahjong(validFigures, Nil))
    }
  }


  test("HuLe Points computer s2s3s4s6s7s8s9s9 c6c7c8b6b7b8") {

    val hule: HuLe = HuLe(
      List(Chow(s2, s3, s4), Chow(s6, s7, s8), Dui(s9)),
      List(Chow(b6, b7, b8), Chow(c6, c7, c8)),
      ContextualTile(b1, SelfDrawn)
    )

    val expected = DetailedPoints(hule,
      List(
        (List(Chow(b6, b7, b8), Chow(c6, c7, c8), Chow(s6, s7, s8)), MixedTripleChow),
        (List(Chow(b6, b7, b8), Chow(c6, c7, c8), Chow(s2, s3, s4), Chow(s6, s7, s8)), AllChows)
      ))

    val actual = HulePointsComputer(hule)

    assert(actual == expected, "actual : " + actual + "\nexpexted : " + expected)
  }

  test("waiting tiles: simple case middle") {
    val waitingFor: List[Tile] = UniqueWait.waitingTiles(TileSet(List(b1, b3)))
    assert(List(b2) === waitingFor)
  }

  test("waiting tiles: simple case left") {
    val waitingFor: List[Tile] = UniqueWait.waitingTiles(TileSet(List(b8, b9)))
    assert(List(b7) === waitingFor)
  }

  test("waiting tiles: simple case right") {
    val waitingFor: List[Tile] = UniqueWait.waitingTiles(TileSet(List(b1, b2)))
    assert(List(b3) === waitingFor)
  }

  test("waiting tiles: double wait") {
    val waitingFor: List[Tile] = UniqueWait.waitingTiles(TileSet(List(b2, b3)))
    assert(List(b1, b4) === waitingFor)
  }

  test("waiting tiles: pair") {
    val waitingFor: List[Tile] = UniqueWait.waitingTiles(TileSet(List(b2)))
    assert(List(b2) === waitingFor)
  }

  test("waiting tiles: hot") {
    val waitingFor: List[Tile] = UniqueWait.waitingTiles(TileSet(List(b5, b6, b7, s2, s3, s3, s3, s4, s5, s6, s7, c5, c6, c7)))
    assert(List(s3) === waitingFor)
  }

  test("waiting on knitted") {
    val waitingFor: List[Tile] = UniqueWait.waitingTiles(TileSet(List(b4, b7, c2, c5, c8, s3, s6, s9)))
    assert(List(b1) === waitingFor)
  }

}
