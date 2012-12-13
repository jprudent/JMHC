package org.liprudent.majiang.mahjong

import org.liprudent.majiang.figures._
import org.liprudent.majiang.figures.Knitted
import org.liprudent.majiang.figures.Dui
import org.liprudent.majiang.tiles.Types.Figures
import scala.Some
import org.liprudent.majiang.figures.SomeKnittedWithSomeDragons
import org.liprudent.majiang.tiles.SelfDrawn

sealed trait Combination {
  val id: Int
  val points: Int
  val name: String
  val description: String
  /**
   * If true, this combination doesn't exclude with the ones where fullHand is false
   */
  val fullHand: Boolean

  def find(m: HuLe): Option[Figures]

  override lazy val toString = "n°%d, %d points, %s".format(id, points, name)
}

object MixedDoubleChows extends Combination {
  val id = 70
  val points = 1
  val name = "Mixes Double Chows"
  val description = "2 identical chows in two families"
  val fullHand = false

  def find(m: HuLe): Option[Figures] = {
    val allMixedChows: List[Figures] =
      for {chow1 <- m.allChows
           chow2 <- m.allChows if chow1 != chow2 && chow1.hasSameValues(chow2)
      } yield (List(chow1, chow2))

    allMixedChows match {
      case Nil => None
      case first :: others => Some(first)
    }
  }
}

object AllChows extends Combination {
  val id = 63
  val points = 2
  val name = "All Chows"
  val description = "All Chows but one Pair"
  val fullHand = true

  def find(m: HuLe): Option[Figures] =
    if (m.allDuis.size == 1 && m.allChows.size == 4)
      Some(m.allChows)
    else
      None
}

object MeldedHand extends Combination {
  val id = 53
  val points = 6
  val name = "Melded Hand"
  val description = "4 figures melded, and finish on discard"
  val fullHand = true

  def find(m: HuLe): Option[Figures] = {
    val cond = m.allFigures.size == 5 && m.disclosed.size == 4 && m.lastTileContext.origin != SelfDrawn
    if (cond)
      Some(m.allFigures)
    else
      None
  }
}

object MixedTripleChow extends Combination {
  val id = 41
  val points = 8
  val name = "Mixed Triple Chow"
  val description = "Three identical chows in three families"
  val fullHand = false

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
  val fullHand = false

  def find(m: HuLe): Option[Figures] = {
    m.closed.find(_.isInstanceOf[Knitted]).map(List(_))
  }
}

object LesserHonorsAndKnittedTiles extends Combination {
  val id = 34
  val points = 12
  val name = "Lesser Honors and Knitted Tiles"
  val description = "6 unique honors and incomplete knitted straight OR 5 unique honors and complete knitted straight"
  val fullHand = true

  def find(m: HuLe): Option[Figures] =
    m.closed.find(_.isInstanceOf[SomeKnittedWithSomeDragons]).map(List(_))
}

object GreaterHonorsAndKnittedTiles extends Combination {
  val id = 20
  val points = 24
  val name = "Greater Honors and Knitted Tiles"
  val description = "The 7 unique honors and incomplete knitted tiles"
  val fullHand = true

  def find(m: HuLe): Option[Figures] =
    m.closed.find {
      figure => figure.isInstanceOf[SomeKnittedWithSomeDragons] &&
        figure.asInstanceOf[SomeKnittedWithSomeDragons].dragons.size == 7
    }.map(List(_))
}

object SevenPairs extends Combination {
  val id = 19
  val points = 24
  val name = "Seven Pairs"
  val description = "7 pairs"
  val fullHand = true

  def find(m: HuLe): Option[Figures] =
    if (m.closed.size == 7 && m.closed.forall(_.isInstanceOf[Dui]))
      Some(m.closed)
    else None
}

object ThirteenOrphansComb extends Combination {
  val id = 7
  val points = 88
  val name = "Thirteen Orphans"
  val description = "1 and 9 in three families and 7 distinct honors plus 1 extra honor"
  val fullHand = true

  def find(m: HuLe): Option[Figures] =
    m.closed.find {
      figure => figure.isInstanceOf[ThirteenOrphans]
    }.map(List(_))
}