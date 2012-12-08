package org.liprudent.majiang

import tiles._
import scala.Some

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
  }

  object AllChows extends Combination {
    val id = 63
    val points = 2
    val name = "All Chows"
    val description = "All Chows but one Pair"

    def find(m: Mahjong): Option[Figures] =
      if(m.allDuis.size == 1 && m.allChows.size == 4)
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
      m.allChows
        .groupBy(_.t1.value)
        .values
        .filter(_.size == 3)
      match {
        case Nil => None
        case x::xs => Some(x::xs)
      }
    }
  }

  type DetailedPoints = List[(List[Figure], Combination)]

  object Points {

    val combinations = List(AllChows, MixedTripleChow)

    def apply(m: Mahjong): DetailedPoints = {
      val res = combinations.map(combination => combination.find(m))
      res.zip(combinations)
        .filter((figures : Option[Figures], combination: Combination) => figures.isDefined)
        .map((figures : Option[Figures], combination: Combination) => (figures.get,combination))
    }

  }

  object MahjongFinder {

    def apply(ptiles: PlayerTiles): List[Figures] = {
      if (!quickValid(ptiles)) Nil
      else Nil
    }

    def quickValid(ptiles: PlayerTiles): Boolean = {
      ptiles.size == 14
    }

  }

}