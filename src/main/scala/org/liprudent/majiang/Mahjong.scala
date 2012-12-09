package org.liprudent.majiang

import scala.Some
import org.liprudent.majiang.figures.{OrdFigure, Dui, Chow, Figure}
import org.liprudent.majiang.tiles.Hand

package object mahjong {

  type Figures = List[Figure]

  case class PlayerTiles(hand: Hand, disclosed: Figures) {
    lazy val size = hand.size + disclosedSize
    lazy val disclosedSize = disclosed.foldLeft(0)((sum: Int, f: Figure) => sum + f.properties.size)
  }

  case class Mahjong(closed: Figures, disclosed: Figures) {
    lazy val allFigures = closed ::: disclosed
    lazy val allChows: List[Chow] = allFigures.filter(_.isInstanceOf[Chow]).asInstanceOf[List[Chow]]
    lazy val allDuis: List[Dui] = allFigures.filter(_.isInstanceOf[Dui]).asInstanceOf[List[Dui]]
  }

  sealed trait Combination {
    val id: Int
    val points: Int
    val name: String
    val description: String

    def find(m: Mahjong): Option[Figures]

    override lazy val toString = "n°%d, %d points, %s".format(id, points, name)
  }

  object AllChows extends Combination {
    val id = 63
    val points = 2
    val name = "All Chows"
    val description = "All Chows but one Pair"

    def find(m: Mahjong): Option[Figures] =
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

    def find(m: Mahjong): Option[Figures] = {
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

  type DetailedPoints = List[(List[Figure], Combination)]

  object Points {

    val combinations = List(AllChows, MixedTripleChow)

    def apply(m: Mahjong): DetailedPoints = {
      val res = combinations.map(combination => combination.find(m))
      val zipped: List[(Option[Figures], Combination)] = res.zip(combinations)
      zipped.filter {
        case (optFigures, _) => optFigures.isDefined
      }
        .map {
        case (optFigures, combination) => (optFigures.get.sorted(OrdFigure), combination)
      }
    }

  }

  case class MahjongFinder(ptiles: PlayerTiles) {

    def find(ptiles: PlayerTiles): List[DetailedPoints] = {
      if (!quickValid) Nil
      else findFigures(ptiles.hand).filter(isWellFormed(_, ptiles.disclosed))
        .map(f => Points(Mahjong(f, ptiles.disclosed)))
    }

    //TODO pour le moment, recherche de 4 figures de 3 tuiles et 1 paire
    def isWellFormed(closed: Figures, disclosed: Figures): Boolean = {
      val all = closed ::: disclosed
      all.size == 5 && all.filter(_.properties.size == 3).size == 4 && all.filter(_.properties.size == 2).size == 1
    }

    //TODO return Set
    private def findFigures(hand: Hand): List[Figures] = {
      hand.allFigures match {
        case f :: Nil => List(List(f))
        case f :: fs => findFigures(hand.remove(f.asList)).map(fs => f :: fs)
      }
    }

    def quickValid: Boolean = {
      ptiles.size == 14
    }


  }

}
