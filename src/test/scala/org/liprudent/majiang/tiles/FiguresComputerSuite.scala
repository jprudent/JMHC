package org.liprudent.majiang.tiles

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import Tile._
import org.liprudent.majiang.tiles.Types._
import org.liprudent.majiang.figures._
import org.liprudent.majiang.figures.Pung
import org.liprudent.majiang.figures.Dui

@RunWith(classOf[JUnitRunner])
class FiguresComputerSuite extends FunSuite {

  test("Find suits") {
    val hand1 = FiguresComputer(List(b1, b2, b3, b1, b2, b2, b2, we, wn, dg, dg))
    val actual: List[Suit] = hand1.findSuits(hand1.tileSet)
    val expected = List(List(b1, b2, b3), List(b1, b2), List(b2), List(b2), List(we), List(wn), List(dg), List(dg))
    assert(actual == expected, actual)
  }

  test("findSubSuitsIndices") {
    val hand1 = FiguresComputer(List(b1, b2, b3, b4))
    val expected = List(List(0, 1, 2), List(1, 2, 3), List(2, 3, 4))
    val actual = hand1.findSubSuitsIndices(5, 3)
    assert(actual == expected, actual)
  }

  test("listOf") {
    val hand1 = FiguresComputer(List(b1, b2, b3, b4))
    val expected = List(List(b1, b2, b3), List(b2, b3, b4))
    val actual = hand1.listsOf(3, List(List(b1, b2, b3, b4)))
    assert(actual == expected, actual)
  }

  test("Split by family") {
    val hand = FiguresComputer(List(b2, c1, b1, s1, c2, dr, dr, we, ww))

    val families: List[List[TileOccurence]] = hand.splitByFamily
    assert(families == List(List((b1, 1), (b2, 1)), List((c1, 1), (c2, 1)), List((s1, 1)),
      List((we, 1)), List((ww, 1)), List((dr, 2))), families)

  }


  test("Chows finding") {
    val hand1 = FiguresComputer(List(b1, b2, b3, b4, we))
    val chows1 = hand1.findChows
    assert(chows1 == List(Chow(b1, b2, b3), Chow(b2, b3, b4)), chows1)

    val hand2 = FiguresComputer(List(b1, b2, b3, b1, b2, ww, ws))
    val chows2 = hand2.findChows
    assert(chows2 == List(Chow(b1, b2, b3)), chows2)

    val hand3 = FiguresComputer(List(b1, b3, b4, dr, dg))
    val chows3 = hand3.findChows
    assert(chows3 == List(), chows3)

    val hand4 = FiguresComputer(List(c1, b3, b2, c2, c3, c7, c8, c9, s1, b1, b1, b3, b2))
    val chows4 = hand4.findChows
    assert(chows4 == List(Chow(b1, b2, b3), Chow(b1, b2, b3), Chow(c1, c2, c3), Chow(c7, c8, c9)), chows4)
  }

  test("Dui finding") {
    val hand1 = FiguresComputer(List(b1, b1, dr, dr, dr, ww, ww, ww, ww))
    val duis1 = hand1.findDuis
    assert(duis1 == List(Dui(b1), Dui(ww), Dui(ww), Dui(dr)), duis1)
  }

  test("Pungs finding") {
    val hand1 = FiguresComputer(List(b1, b3, b1, b4, b5, b4, b4, b1))
    val pungs1 = hand1.findPungs
    assert(pungs1 == List(Pung(b1), Pung(b4)), pungs1)

    val hand2 = FiguresComputer(List(b1, b2, b3, b1))
    val pungs2 = hand2.findPungs
    assert(pungs2 == List(), pungs2)
  }

  test("All figures") {
    val hand = FiguresComputer(List(b1, b2, b3, b1, b2, b3, b1, b2, b3))
    val expected = List(Pung(b1), Pung(b2), Pung(b3), Chow(b1, b2, b3), Chow(b1, b2, b3), Chow(b1, b2, b3), Dui(b1), Dui(b2), Dui(b3))
    val actual = hand.allFigures
    assert(expected == actual, actual)
  }


  test("find all combinations of figures when there is one") {
    val hand = FiguresComputer(List(b2, b3, b1))
    val expected = Set(List(Chow(b1, b2, b3)))
    val actual = hand.allFiguresCombinations
    assert(actual == expected, actual)
  }

  test("find all combinations of figures when there are two") {
    val expected = List(
      List(Chow(b1, b2, b3), Chow(b1, b2, b3), Chow(b1, b2, b3)),
      List(Pung(b1), Pung(b2), Pung(b3)))

    val hand = FiguresComputer(List(b1, b2, b3, b1, b2, b3, b1, b2, b3))
    val actual = hand.allFiguresCombinations

    def nicePrint(lstFigures: Set[Figures]) {
      lstFigures.foreach {
        figures => println("Solution : ")
        figures.foreach(figure => println("\t" + figure))
      }
    }

    nicePrint(actual)

    assert(expected.forall(exp => actual.contains(exp)), actual)
  }


}
