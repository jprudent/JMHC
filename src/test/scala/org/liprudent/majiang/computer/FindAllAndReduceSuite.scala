package org.liprudent.majiang.computer

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.liprudent.majiang.figures._
import org.liprudent.majiang.tiles.Tile._
import org.liprudent.majiang.figures.ThirteenOrphans
import org.liprudent.majiang.figures.Knitted
import org.liprudent.majiang.figures.Pung
import org.liprudent.majiang.figures.Dui
import org.liprudent.majiang.figures.SomeKnittedWithSomeDragons
import org.liprudent.majiang.tiles.{TocTileSet, Stone, Bamboo, Character}

@RunWith(classOf[JUnitRunner])
class FindAllAndReduceSuite extends FunSuite {

  test("Find suits") {
    val hand1 = TocTileSet(List(b1, b2, b3, b1, b2, b2, b2, we, wn, dg, dg))
    val actual: List[Suit] = FindAllAndReduce.findSuits(hand1)
    val expected = List(List(b1, b2, b3), List(b1, b2), List(b2), List(b2), List(we), List(wn), List(dg), List(dg))
    assert(actual == expected, actual)
  }

  test("findSubSuitsIndices") {
    val expected = List(List(0, 1, 2), List(1, 2, 3), List(2, 3, 4))
    val actual = FindAllAndReduce.findSubSuitsIndices(5, 3)
    assert(actual == expected, actual)
  }

  test("listOf") {
    val expected = List(List(b1, b2, b3), List(b2, b3, b4))
    val actual = FindAllAndReduce.sublistsOf(3, List(List(b1, b2, b3, b4)))
    assert(actual == expected, actual)
  }

  test("Chows finding") {
    val hand1 = FindAllAndReduce(List(b1, b2, b3, b4, we))
    val chows1 = hand1.chows
    assert(chows1 == List(Chow(b1, b2, b3), Chow(b2, b3, b4)), chows1)

    val hand2 = FindAllAndReduce(List(b1, b2, b3, b1, b2, ww, ws))
    val chows2 = hand2.chows
    assert(chows2 == List(Chow(b1, b2, b3)), chows2)

    val hand3 = FindAllAndReduce(List(b1, b3, b4, dr, dg))
    val chows3 = hand3.chows
    assert(chows3 == List(), chows3)

    val hand4 = FindAllAndReduce(List(c1, b3, b2, c2, c3, c7, c8, c9, s1, b1, b1, b3, b2))
    val chows4 = hand4.chows
    assert(chows4 == List(Chow(b1, b2, b3), Chow(b1, b2, b3), Chow(c1, c2, c3), Chow(c7, c8, c9)), chows4)
  }

  test("Dui finding") {
    val hand1 = FindAllAndReduce(List(b1, b1, dr, dr, dr, ww, ww, ww, ww))
    val duis1 = hand1.duis
    assert(duis1 == List(Dui(b1), Dui(ww), Dui(ww), Dui(dr)), duis1)
  }

  test("Pungs finding") {
    val hand1 = FindAllAndReduce(List(b1, b3, b1, b4, b5, b4, b4, b1))
    val pungs1 = hand1.pungs
    assert(pungs1 == List(Pung(b1), Pung(b4)), pungs1)

    val hand2 = FindAllAndReduce(List(b1, b2, b3, b1))
    val pungs2 = hand2.pungs
    assert(pungs2 == List(), pungs2)
  }

  test("Knitted straight") {
    val computer = FindAllAndReduce(List(c1, c4, c7, b2, b5, b8, s3, s6, s9))
    val knitted = computer.knitted
    assert(knitted === List(Knitted(Character, Bamboo, Stone)))
  }

  test("Knitted straight 2") {
    val computer = FindAllAndReduce(List(b1, b4, b7, c2, c5, c8, s3, s6, s9))
    val knitted = computer.knitted
    assert(knitted === List(Knitted(Bamboo, Character, Stone)))
  }

  test("Some honors and some knitted") {
    val computer = FindAllAndReduce(List(b1, b4, b7, c2, c5, c8, s3, s6, s9, dr, dg, dw, ww, we))
    val someHonorsSomeKnitted = computer.someKnittedSomeDragons
    assert(someHonorsSomeKnitted === List(SomeKnittedWithSomeDragons(
      List(b1, b4, b7, c2, c5, c8, s3, s6, s9), List(we, ww, dr, dg, dw))))
  }

  test("13 orphans") {
    val computer = FindAllAndReduce(List(b1, b9, c1, c9, s1, s9, dr, dg, dw, ww, we, ws, wn, wn))
    val thirteenOrphans = computer.thirteenOrphans
    assert(thirteenOrphans === List(ThirteenOrphans(wn)))
  }

  test("not 13 orphans") {
    val computer = FindAllAndReduce(List(b2, b9, c1, c9, s1, s9, dr, dg, dw, ww, we, ws, wn, wn))
    val thirteenOrphans = computer.thirteenOrphans
    assert(thirteenOrphans === List())
  }

  test("All figures") {
    val hand = FindAllAndReduce(List(b1, b2, b3, b1, b2, b3, b1, b2, b3))
    val expected = List(Pung(b1), Pung(b2), Pung(b3), Chow(b1, b2, b3), Chow(b1, b2, b3), Chow(b1, b2, b3), Dui(b1), Dui(b2), Dui(b3))
    val actual = hand.allFigures
    assert(expected === actual)
  }


  test("find all combinations of figures when there is one") {
    val hand = FindAllAndReduce(List(b2, b3, b1))
    val expected = Set(List(Chow(b1, b2, b3)))
    val actual = hand.allFiguresCombinations
    assert(actual == expected, actual)
  }

  test("find all combinations of figures when there are two") {
    val expected = List(
      List(Chow(b1, b2, b3), Chow(b1, b2, b3), Chow(b1, b2, b3)),
      List(Pung(b1), Pung(b2), Pung(b3)))

    val hand = FindAllAndReduce(List(b1, b2, b3, b1, b2, b3, b1, b2, b3))
    val actual = hand.allFiguresCombinations

    assert(expected.forall(exp => actual.contains(exp)), actual)
  }


}
