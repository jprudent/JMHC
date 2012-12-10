package org.liprudent.majiang

import scala.Some
import org.liprudent.majiang.figures.{OrdFigure, Dui, Chow, Figure}
import org.liprudent.majiang.tiles.Hand
import collection.immutable.Set

package object mahjong {

  type Figures = List[Figure]

  case class PlayerTiles(hand: Hand, disclosed: Figures) {
    lazy val size = hand.size + disclosedSize
    lazy val disclosedSize = disclosed.foldLeft(0)((sum: Int, f: Figure) => sum + f.properties.size)
  }

  case class HuLe(closed: Figures, disclosed: Figures) {
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

  case class DetailedPoints(huLe: HuLe, detailedPoints: List[(List[Figure], Combination)]) {
    override def toString = {
      val title = "For Hule : " + huLe + "\n"
      val detail = detailedPoints.foldLeft("")((string, line) => {
        line._2 + ":" + line._1 + "\n"
      })
      title + detail
    }
  }

  /**
   * Ordering is done on Combination.points, then Combination.id
   */
  object OrdDetailedPoint extends Ordering[(List[Figure], Combination)] {
    override def compare(x: (List[Figure], Combination), y: (List[Figure], Combination)) = {
      x._2.points.compare(y._2.points) match {
        case 0 => x._2.id.compare(y._2.id)
        case n => n
      }
    }
  }

  object Points {

    val combinations = List(AllChows, MixedTripleChow)

    def apply(m: HuLe): DetailedPoints = {
      val res = combinations.map(combination => combination.find(m))
      val zipped: List[(Option[Figures], Combination)] = res.zip(combinations)
      val detailedPoints = zipped.filter {
        case (optFigures, _) => optFigures.isDefined
      }
        .map {
        case (optFigures, combination) => (optFigures.get.sorted(OrdFigure), combination)
      }
      DetailedPoints(m, detailedPoints)
    }

  }

  case class MahjongFinder(ptiles: PlayerTiles) {

    def find: List[DetailedPoints] = {
      if (!quickValid) Nil
      else MahjongFinder.findFigures(ptiles.hand)
        .filter(closed => MahjongFinder.isWellFormedMahjong(closed, ptiles.disclosed))
        .map(closed => Points(HuLe(closed, ptiles.disclosed)))
        .toList
      //.sorted(OrdDetailedPoint)

    }

    def quickValid: Boolean = {
      ptiles.size == 14
    }

  }

  object MahjongFinder {
    //TODO pour le moment, recherche de 4 figures de 3 tuiles et 1 paire
    def isWellFormedMahjong(closed: Figures, disclosed: Figures): Boolean = {
      val all = closed ::: disclosed
      all.size == 5 && all.filter(_.properties.size == 3).size == 4 && all.filter(_.properties.size == 2).size == 1
    }


    //TODO optimization: it's useless to try all combinations for a given Figure type
    //List(Chow(b1,b2,b3), Chow(b2,b3,b4)) == List(Chow(b2,b3,b4), Chow(b1,b2,b3))
    def findFigures(hand: Hand): Set[Figures] = {
      hand.allFigures match {
        case Nil => Set()
        case all => {
          all.map {
            figure => {
              val next: Set[Figures] = findFigures(hand.remove(figure.asList))
              next match {
                case s if s.isEmpty => Set(List(figure))
                case _ => next.map(figures => (figure :: figures))
              }
            }
          }
            .flatten
            .toSet[Figures]
            .map(figures => figures.sorted(OrdFigure))
        }
      }
    }

  }

}
