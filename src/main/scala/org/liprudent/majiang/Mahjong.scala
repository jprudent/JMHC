package org.liprudent.majiang

import mahjong.{HuLe, HulePointsComputer, DetailedPoints, PlayerTiles}
import org.liprudent.majiang.figures._
import tiles._
import tiles.ContextualTile
import tiles.Types.Figures
import scala.Some
import org.liprudent.majiang.figures.Dui

package object mahjong {

  case class PlayerTiles(hand: Hand, disclosed: Figures) {
    lazy val size = hand.tileSet.size + disclosedSize
    lazy val disclosedSize = disclosed.foldLeft(0)((sum: Int, f: Figure) => sum + f.properties.size)
  }

  case class HuLe(closed: Figures, disclosed: Figures, lastTileContext: ContextualTile) {
    lazy val allFigures = closed ::: disclosed
    lazy val allChows: List[Chow] = allFigures.filter(_.isInstanceOf[Chow]).asInstanceOf[List[Chow]]
    lazy val allDuis: List[Dui] = allFigures.filter(_.isInstanceOf[Dui]).asInstanceOf[List[Dui]]
  }

  sealed trait Combination {
    val id: Int
    val points: Int
    val name: String
    val description: String

    def find(m: HuLe): Option[Figures]

    override lazy val toString = "n°%d, %d points, %s".format(id, points, name)
  }

  object AllChows extends Combination {
    val id = 63
    val points = 2
    val name = "All Chows"
    val description = "All Chows but one Pair"

    def find(m: HuLe): Option[Figures] =
      if (m.allDuis.size == 1 && m.allChows.size == 4)
        Some(m.allChows)
      else
        None
  }

  object MixedTripleChow extends Combination {
    val id = 41
    val points = 8
    val name = "Mixed Triple Chow"
    val description = "Three identical chows in three families"

    def find(m: HuLe): Option[Figures] = {
      val resolved: List[Figure] = m.allChows
        .groupBy(_.t1.value) // Map[Int, List[Chow]
        .values.toList // List[List[Chow]]
        .filter(_.size == 3)
        .flatten // List[Chow]

      resolved match {
        case Nil => None
        case xs => Some(xs)
      }

    }
  }

  object KnittedStraight extends Combination {
    val id = 35
    val points = 12
    val name = "Knitted Straight"
    val description = "1-4-7 in one family, 2-5-8 in the second family, 3-6-9 in the third family"

    def find(m: HuLe): Option[Figures] = {
      m.closed.find(_.isInstanceOf[Knitted]).map(List(_))
    }
  }

  object LesserHonorsAndKnittedTiles extends Combination {
    val id = 34
    val points = 12
    val name = "Lesser Honors and Knitted Tiles"
    val description = "6 unique honors and incomplete knitted straight OR 5 unique honors and complete knitted straight"

    def find(m: HuLe): Option[Figures] =
      m.closed.find(_.isInstanceOf[SomeKnittedWithSomeDragons]).map(List(_))
  }

  object GreaterHonorsAndKnittedTiles extends Combination {
    val id = 20
    val points = 24
    val name = "Greater Honors and Knitted Tiles"
    val description = "The 7 unique honors and incomplete knitted tiles"

    def find(m: HuLe): Option[Figures] =
      m.closed.find {
        figure => figure.isInstanceOf[SomeKnittedWithSomeDragons] &&
          figure.asInstanceOf[SomeKnittedWithSomeDragons].dragons.size == 7
      }.map(List(_))
  }

  case class DetailedPoints(huLe: HuLe, detailedPoints: List[(List[Figure], Combination)]) {
    override def toString = {
      val title = "For Hule : " + huLe + "\n"
      val detail = detailedPoints.foldLeft("")((string, line) => {
        string + line._2 + ":" + line._1 + "\n"
      })
      title + detail
    }
  }

  /**
   * Ordering is done on Combination.points descending, then Combination.id ascending
   */
  object OrdDetailedPoint extends Ordering[(List[Figure], Combination)] {
    override def compare(x: (List[Figure], Combination), y: (List[Figure], Combination)) = {
      y._2.points.compare(x._2.points) match {
        case 0 => x._2.id.compare(y._2.id)
        case n => n
      }
    }
  }

  object HulePointsComputer {

    //TODO uncomment when all combinations have been implemented
    //require(combinations.size == 88)

    val combinations = List(
      AllChows,
      MixedTripleChow,
      KnittedStraight,
      LesserHonorsAndKnittedTiles,
      GreaterHonorsAndKnittedTiles)


    def apply(huLe: HuLe): DetailedPoints = {
      val res = combinations.map(combination => combination.find(huLe))
      val zipped: List[(Option[Figures], Combination)] = res.zip(combinations)
      val allDetailedPoints = zipped.filter {
        case (optFigures, _) => optFigures.isDefined
      }
        .map {
        case (optFigures, combination) => (optFigures.get.sorted(OrdFigure), combination)
      }
        .sorted(OrdDetailedPoint)

      val detailedPoints = applyExclusion(allDetailedPoints)

      DetailedPoints(huLe, detailedPoints)
    }


    private def applyExclusion(allDetailedPoints: List[(Figures, Combination)]): List[(Figures, Combination)] = {
      allDetailedPoints match {
        case Nil => Nil
        case (figures, combination) :: t =>
          (figures, combination) :: applyExclusion(t.filter {
            case (tailFigures, comb) => tailFigures != figures
          })
      }
    }
  }

  private def countFiguresUsed(allDetailedPoints: List[(Figures, Combination)]): Map[Figure, Int] = {
    allDetailedPoints
      .map {
      case (figures, combination) => figures
    }
      .foldLeft(Map[Figure, Int]())((map, figures) =>
      figures.foldLeft(map)((map, figure) => map.updated(figure, map.getOrElse(figure, 0) + 1)))
  }

}

case class HuLeFinder(ptiles: PlayerTiles) {

  /**
   *
   * @return A list. Each element is a detailed solution for given <code>ptiles</code>.
   */
  lazy val find: List[DetailedPoints] = {
    if (!quickValid) Nil
    else {
      val computer = FiguresComputer(ptiles.hand.tileSet)
      computer.allFiguresCombinations
        .filter(closed => HuLeFinder.isWellFormedMahjong(closed, ptiles.disclosed))
        .map(closed => HulePointsComputer(HuLe(closed, ptiles.disclosed, ptiles.hand.lastTileContext)))
        .toList
    }
  }

  lazy val quickValid = ptiles.size == 14

}

object HuLeFinder {
  //TODO pour le moment, recherche de 4 figures de 3 tuiles et 1 paire
  def isWellFormedMahjong(closed: Figures, disclosed: Figures): Boolean = {
    val all = closed ::: disclosed
    //classical mahjong hand
    classicalMahjondHand(all) ||
      //knitted staight hand
      knittedStraightHand(all) ||
      //partial or complete knitted + unique dragons
      someKnittedSomeDragons(closed)
  }


  def someKnittedSomeDragons(closed: List[figures.Figure]): Boolean = {
    closed.size == 1 &&
      closed(0).isInstanceOf[SomeKnittedWithSomeDragons]
  }

  def knittedStraightHand(all: List[figures.Figure]): Boolean = {
    all.size == 3 && all.exists(_.isInstanceOf[Knitted]) &&
      all.filter(_.properties.size == 3).size == 1 &&
      all.filter(_.properties.size == 2).size == 1
  }


  def classicalMahjondHand(all: List[figures.Figure]): Boolean = {
    all.size == 5 && all.filter(_.properties.size == 3).size == 4 && all.filter(_.properties.size == 2).size == 1
  }
}

object UniqueWait {

  def waitingTiles(tileSet: TileSet): List[Tile] = {

    def completeCombination(figures: Figures, tileSet: TileSet) =
      Figures.size(figures) == tileSet.size

    def satisfy(tile: Tile): Boolean = {
      val added: TileSet = tileSet.added(tile)
      val allCombinations = FiguresComputer(added).allFiguresCombinations
      allCombinations.filter(figures => completeCombination(figures, added)).size == 1
    }

    Tile.all.filter(satisfy).toList.sorted
  }

}
