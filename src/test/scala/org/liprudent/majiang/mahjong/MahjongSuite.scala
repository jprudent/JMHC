package org.liprudent.majiang.mahjong

import org.liprudent.majiang.tiles._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import Tile._
import org.liprudent.majiang.figures._
import org.liprudent.majiang.figures.Knitted
import org.liprudent.majiang.figures.Pung
import org.liprudent.majiang.figures.Dui
import org.liprudent.majiang.tiles.ContextualTile
import org.liprudent.majiang.{HuLeFinder, UniqueWait}

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
    val knittedStraightLesserDragon5 = PlayerTiles(
      Hand(List(b1, b4, b7, s2, s5, s8, c3, c6, c9, ww, we, ws, wn, dr), ContextualTile(b1, Discarded)),
      Nil
    )
    val knittedStraightLesserDragon6 = PlayerTiles(
      Hand(List(b1, b4, b7, s2, s5, s8, c6, c9, ww, we, ws, wn, dr, dg), ContextualTile(b1, Discarded)),
      Nil
    )
    val greaterHonorsAndKnittedTiles = PlayerTiles(
      Hand(List(b1, b4, b7, s2, s5, s8, c9, ww, we, ws, wn, dr, dg, dw), ContextualTile(b1, Discarded)),
      Nil
    )
    val thirteenOrphans = PlayerTiles(
      Hand(List(b1, b9, c1, c9, s1, s9, we, wn, ww, ws, dr, dg, dw, dr), ContextualTile(b1, Discarded)),
      Nil
    )
    val sevenPairs = PlayerTiles(
      Hand(List(b1, b1, b2, b2, s4, s4, we, we, b8, b8, ww, ww, ww, ww), ContextualTile(b1, Discarded)),
      Nil
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
      assert(actual === expected)
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

  test("HuLe Finder knitted straight lesser dragon (5): b1b4b7s2s5s8c3c6c9wwwewswndr") {
    new Hands {
      val actual = HuLeFinder(knittedStraightLesserDragon5).find
      val expected = List(
        DetailedPoints(
          HuLe(List(SomeKnittedWithSomeDragons(List(b1, b4, b7, c3, c6, c9, s2, s5, s8), List(we, wn, ww, ws, dr))),
            Nil,
            ContextualTile(b1, Discarded)),
          List(
            (List(SomeKnittedWithSomeDragons(List(b1, b4, b7, c3, c6, c9, s2, s5, s8), List(we, wn, ww, ws, dr))), LesserHonorsAndKnittedTiles)
          )
        )
      )
      assert(actual === expected)
    }
  }

  test("HuLe Finder knitted straight lesser dragon (6): b1b4b7s2s5s8c3c6c9wwwewswndr") {
    new Hands {
      val actual = HuLeFinder(knittedStraightLesserDragon6).find

      val expected = List(
        DetailedPoints(
          HuLe(List(SomeKnittedWithSomeDragons(List(b1, b4, b7, c6, c9, s2, s5, s8), List(we, wn, ww, ws, dr, dg))),
            Nil,
            ContextualTile(b1, Discarded)),
          List(
            (List(SomeKnittedWithSomeDragons(List(b1, b4, b7, c6, c9, s2, s5, s8), List(we, wn, ww, ws, dr, dg))), LesserHonorsAndKnittedTiles)
          )
        )
      )
      assert(actual === expected)
    }
  }

  test("HuLe Finder greater honors and knitted tiles") {
    new Hands {
      val actual = HuLeFinder(greaterHonorsAndKnittedTiles).find

      val expected = List(
        DetailedPoints(
          HuLe(List(SomeKnittedWithSomeDragons(List(b1, b4, b7, c9, s2, s5, s8), List(we, wn, ww, ws, dr, dg, dw))),
            Nil,
            ContextualTile(b1, Discarded)),
          List(
            (List(SomeKnittedWithSomeDragons(List(b1, b4, b7, c9, s2, s5, s8), List(we, wn, ww, ws, dr, dg, dw))), GreaterHonorsAndKnittedTiles)
          )
        )
      )
      assert(actual === expected)
    }
  }

  test("HuLe Finder 13 orphans") {
    new Hands {
      val actual = HuLeFinder(thirteenOrphans).find

      val expected = List(
        DetailedPoints(
          HuLe(List(ThirteenOrphans(dr)),
            Nil,
            ContextualTile(b1, Discarded)),
          List(
            (List(ThirteenOrphans(dr)), ThirteenOrphansComb)
          )
        )
      )
      assert(actual === expected)
    }
  }

  test("HuLe Finder 7 pairs") {
    new Hands {
      val actual = HuLeFinder(sevenPairs).find

      val expected = List(
        DetailedPoints(
          HuLe(List(Dui(b1), Dui(b2), Dui(b8), Dui(s4), Dui(we), Dui(ww), Dui(ww)),
            Nil,
            ContextualTile(b1, Discarded)),
          List(
            (List(Dui(b1), Dui(b2), Dui(b8), Dui(s4), Dui(we), Dui(ww), Dui(ww)), SevenPairs)
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


  test("waiting tiles: simple case middle") {
    val waitingFor: List[Tile] = UniqueWait.waitingTiles(TileSet(List(b1, b3, c2, c2, c2, ww, ww, ww, we, we, we, dr, dr)), Nil)
    assert(List(b2) === waitingFor)
  }

  test("waiting tiles: simple case left") {
    val waitingFor: List[Tile] = UniqueWait.waitingTiles(TileSet(List(b8, b9, c2, c2, c2, ww, ww, ww, we, we, we, dr, dr)), Nil)
    assert(List(b7) === waitingFor)
  }

  test("waiting tiles: simple case right") {
    val waitingFor: List[Tile] = UniqueWait.waitingTiles(TileSet(List(b1, b2, c2, c2, c2, ww, ww, ww, we, we, we, dr, dr)), Nil)
    assert(List(b3) === waitingFor)
  }

  test("waiting tiles: double wait") {
    val waitingFor: List[Tile] = UniqueWait.waitingTiles(TileSet(List(b2, b3, c2, c2, c2, ww, ww, ww, we, we, we, dr, dr)), Nil)
    assert(List(b1, b4) === waitingFor)
  }

  test("waiting tiles: pair") {
    val waitingFor: List[Tile] = UniqueWait.waitingTiles(TileSet(List(b2, c2, c2, c2, ww, ww, ww, we, we, we, dr, dr, dr)), Nil)
    assert(List(b2) === waitingFor)
  }

  test("waiting tiles: hot") {
    val waitingFor: List[Tile] = UniqueWait.waitingTiles(TileSet(List(b5, b6, b7, s2, s3, s3, s4, s5, s6, s7, c5, c6, c7)), Nil)
    assert(List(s3) === waitingFor)
  }

  test("waiting on knitted straitght") {
    val waitingFor: List[Tile] = UniqueWait.waitingTiles(TileSet(List(b4, b7, c2, c5, c8, s3, s6, s9)), List(Pung(dr), Dui(b4)))
    assert(List(b1) === waitingFor)
  }

  test("waiting on lesser honor and knitted straitght 1") {
    val waitingFor: List[Tile] = UniqueWait.waitingTiles(
      TileSet(List(b1, b4, b7, c2, c5, c8, s3, s6, s9, dr, dg, dw, ww)), Nil)
    assert(List(we, wn, ws) === waitingFor)
  }

  test("waiting on lesser honor and knitted straitght 2") {
    val waitingFor: List[Tile] = UniqueWait.waitingTiles(
      TileSet(List(b1, b4, b7, c2, c8, s3, s6, s9, dr, dg, dw, ww, we)), Nil)
    assert(List(c5, wn, ws) === waitingFor)
  }

  test("waiting on lesser honor and knitted tiles") {
    val waitingFor: List[Tile] = UniqueWait.waitingTiles(
      TileSet(List(b1, b4, b7, c2, c5, c8, s3, ww, we, ws, wn, dr, dg)), Nil)
    assert(List(s6, s9, dw) === waitingFor)
  }

  test("waiting on 13 orphans 1") {
    val waitingFor: List[Tile] = UniqueWait.waitingTiles(
      TileSet(List(b1, b9, c1, c9, s1, s9, ww, we, ws, wn, dr, dg, dw)), Nil)
    assert(List(we, wn, ww, ws, dr, dg, dw) === waitingFor)
  }

  test("waiting on 7 pairs") {
    val waitingFor: List[Tile] = UniqueWait.waitingTiles(
      TileSet(List(b1, b1, c1, c1, s1, s1, ww, ww, ws, ws, dr, dr, dw)), Nil)
    assert(List(dw) === waitingFor)
  }

  test("no waiting") {
    val waitingFor: List[Tile] = UniqueWait.waitingTiles(
      TileSet(List(b1, b1, c1, c1, b1, c1, ww, ww, ws, ws, dr, dr, dw)), Nil)
    assert(List() === waitingFor)
  }

  test("is excluded : greater honors and knitted tiles - Lesser Honors and knitted tiles") {
    val ref = (List(SomeKnittedWithSomeDragons(List(b1, b4, b7, c9, s2, s5, s8), List(we, wn, ww, ws, dr, dg, dw))),
      GreaterHonorsAndKnittedTiles)
    val excl = (List(SomeKnittedWithSomeDragons(List(b1, b4, b7, c9, s2, s5, s8), List(we, wn, ww, ws, dr, dg, dw))),
      LesserHonorsAndKnittedTiles)

    assert(HulePointsComputer.isExcluded(ref, excl))

  }

  test("is not excluded : All Chows - Mixed Triple Chow") {

    val ref = (List(Chow(b1, b2, b3), Chow(b7, b8, b9), Chow(c7, c8, c9), Chow(s7, s8, s9)), AllChows)
    val excl = (List(Chow(b7, b8, b9), Chow(c7, c8, c9), Chow(s7, s8, s9)), MixedTripleChow)

    assert(!HulePointsComputer.isExcluded(ref, excl))

  }

  test("is excluded : Mixed Triple Chow - Mixed Double Chow") {
    val ref = (List(Chow(b7, b8, b9), Chow(c7, c8, c9), Chow(s7, s8, s9)), MixedTripleChow)
    val excl = (List(Chow(b7, b8, b9), Chow(c7, c8, c9)), MixedDoubleChows)

    assert(HulePointsComputer.isExcluded(ref, excl))
  }

}
