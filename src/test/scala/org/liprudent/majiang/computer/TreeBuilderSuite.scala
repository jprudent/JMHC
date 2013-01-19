package org.liprudent.majiang.computer

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.liprudent.majiang.figures._
import org.liprudent.majiang.tiles.Tile._
import org.liprudent.majiang.figures.ThirteenOrphans
import org.liprudent.majiang.figures.Knitted
import org.liprudent.majiang.figures.Pung
import org.liprudent.majiang.figures.SomeKnittedWithSomeDragons
import org.liprudent.majiang.tiles._
import org.liprudent.majiang.figures.Dui
import org.liprudent.majiang.figures.SingleTile

@RunWith(classOf[JUnitRunner])
class TreeBuilderSuite extends FunSuite {

  test("addTile singleTile straight") {
    val figure = SingleTile(b1)

    assert(FigureEnhancerService.addTile(figure, b1) === Some(Dui(b1)))
    assert(FigureEnhancerService.addTile(figure, b2) === Some(new PartialFigure(List(b1, b2), Set(PartialChow))))
    assert(FigureEnhancerService.addTile(figure, b3) === None)
    assert(FigureEnhancerService.addTile(figure, b4) === Some(new PartialFigure(List(b1, b4), Set(PartialKnitted))))
    assert(FigureEnhancerService.addTile(figure, c1) === Some(new PartialFigure(List(b1, c1),
      Set(PartialThirteenOrphans))))
    assert(FigureEnhancerService.addTile(figure, c9) === Some(new PartialFigure(List(b1, c9), Set(PartialKnitted,
      PartialThirteenOrphans))))
    assert(FigureEnhancerService.addTile(figure, dr) === Some(new PartialFigure(List(b1, dr),
      Set(PartialSomeKnittedWithSomeDragons, PartialThirteenOrphans))))

  }


  test("addTile Dui") {
    val figure = Dui(b1)

    assert(FigureEnhancerService.addTile(figure, b1) === Some(Pung(b1)))
    assert(FigureEnhancerService.addTile(figure, b2) === None)
    assert(FigureEnhancerService.addTile(figure, b9) === Some(new PartialFigure(List(b1, b1, b9),
      Set(PartialThirteenOrphans))))
  }

  test("addTile Chow") {
    Tile.allStraight.filter(_.value <= 7).foreach {
      tile =>
        Tile.allButBonus.foreach(added =>
          assert(FigureEnhancerService.addTile(Chow(tile), added) === None))
    }
  }

  test("addTile Pung") {
    Tile.allButBonus.foreach {
      tile =>
        Tile.allButBonus.foreach(added =>
          assert(FigureEnhancerService.addTile(Pung(tile), added) === None))
    }
  }

  test("addTile Kong") {
    intercept[IllegalArgumentException](FigureEnhancerService.addTile(Kong(b1), b2))
  }

  test("addTile Knitted") {
    val figure = Knitted.allPossible.head

    Tile.allHonors.foreach {
      honor =>
        assert(FigureEnhancerService.addTile(figure, honor) === Some(new PartialFigure(figure.toTiles ::: List(honor)
          , Set(PartialSomeKnittedWithSomeDragons))))
    }

    Tile.allStraight.foreach {
      straight =>
        assert(FigureEnhancerService.addTile(figure, straight) === None)
    }
  }

  //TODO tests PartialFigure

  test("addTile someKnittedWithSomeDragons") {
    val figure: SomeKnittedWithSomeDragons = SomeKnittedWithSomeDragons(List(b1, b2, b3, c1, s1, s2, s3), List(we,
      wn, ww, ws, dr, dg, dw))
    assert(FigureEnhancerService.addTile(figure, c2) === None)
  }

  test("addTile ThirteenOrphans") {
    //thirteen orphan cannot be enhanced
    val allThirteenOrphans = Tile.allTerminalsOrHonors.map(t => ThirteenOrphans(t))
    allThirteenOrphans.foreach(thirteenOrphan =>
      Tile.all.foreach(tile => assert(FigureEnhancerService.addTile(thirteenOrphan, tile) === None)))
  }

  test("addTile PartialFigure") {
    val partialChow = new PartialFigure(List(b1, b2), Set(PartialChow))
    assert(FigureEnhancerService.addTile(partialChow, b3) === Some(Chow(b1)))
    assert(FigureEnhancerService.addTile(partialChow, b4) === None)

    val partialKnitted = new PartialFigure(List(b2, b5, b8, c1, c7, s3, s6, s9), Set(PartialKnitted))
    assert(FigureEnhancerService.addTile(partialKnitted, c4) === Some(Knitted(Character, Bamboo, Stone)))
    assert(FigureEnhancerService.addTile(partialChow, b4) == None)
    assert(FigureEnhancerService.addTile(partialKnitted, dr) === Some(PartialFigure(List(b2, b5, b8, c1, c7, s3, s6,
      s9, dr), Set(PartialSomeKnittedWithSomeDragons))))

    val partialSKnSDr = new PartialFigure(List(b2, b5, b8, c1, c7, s3, s6, s9, we, wn, ww, dr, dg),
      Set(PartialSomeKnittedWithSomeDragons))
    assert(FigureEnhancerService.addTile(partialSKnSDr, dw) ===
      Some(SomeKnittedWithSomeDragons(List(b2, b5, b8, c1, c7, s3, s6, s9), List(we, wn, ww, dr, dg, dw))))
    assert(FigureEnhancerService.addTile(partialSKnSDr, b4) === None)

    val partial13Or = new PartialFigure(List(b1, b9, c1, c9, s1, s9, we, wn, ww, ws, dr, dg, dw),
      Set(PartialThirteenOrphans))
    assert(FigureEnhancerService.addTile(partial13Or, b9) === Some(ThirteenOrphans(b9)))
    assert(FigureEnhancerService.addTile(partial13Or, b4) === None)

  }
}
