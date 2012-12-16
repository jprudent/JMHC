package org.liprudent.majiang.mahjong

import org.liprudent.majiang.figures._
import org.liprudent.majiang.tiles.Types.Figures
import org.liprudent.majiang.tiles._
import org.liprudent.majiang.UniqueWait
import org.liprudent.majiang.figures.ThirteenOrphans
import org.liprudent.majiang.figures.Knitted
import org.liprudent.majiang.figures.Dui
import scala.Some
import org.liprudent.majiang.figures.SomeKnittedWithSomeDragons

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

  /**
   * List of implied combinations by this one
   */
  val implied = List[Combination]()

  /**
   * Check wether x implies y
   * @param x this combination with these figures
   * @param y the other combination with those figures
   * @return true if y is implied by x. false otherwise
   */
  def imply(x: Figures, y: (Figures, Combination)): Boolean = {
    def containsAll(x: Figures, y: Figures): Boolean =
      y.forall(figure => x.contains(figure))
    implied.exists(combination => combination == y._2 && containsAll(x, y._1))
  }

  override lazy val toString = "n°%d, %d points, %s".format(id, points, name)
}

object FlowerTiles extends Combination {
  val id = 81
  val points = 1
  val name = "Flower Tiles"
  val description = "Flowers and Seasons Tiles"
  val fullHand = false

  def find(m: HuLe): Option[Figures] = {
    m.bonus.bonus match {
      case Nil => None
      case _ => Some(List(m.bonus))
    }
  }

}

object SelfDrawnComb extends Combination {
  val id = 80
  val points = 1
  val name = "Self Drawn"
  val description = "finish with self drawing"
  val fullHand = false

  override def find(m: HuLe): Option[Figures] = {
    m.lastTileContext.origin == SelfDrawn
    match {
      case true =>
        m.closed.find(_.asList.contains(m.lastTileContext.tile)).map(List(_))
      case false => None
    }
  }

}

sealed trait WaitCombination extends Combination {

  def matchingWait(figure: Figure, waitingTile: Tile): Boolean

  def find(m: HuLe): Option[Figures] = {
    if (m.lastTileContext.origin != Discarded) None
    else {
      val closedTiles = m.closed.map(_.asList).flatten
      val tilesBeforeWinning: TileSet = TileSet(closedTiles).removed(m.lastTileContext.tile)
      val waitingTiles = UniqueWait.waitingTiles(tilesBeforeWinning, m.disclosed)
      println(waitingTiles)

      waitingTiles match {
        case w :: Nil => {
          m.closed.find(figure => matchingWait(figure, w)).map(List(_))
        }
        case _ => None
      }
    }
  }
}


object SingleWait extends Combination with WaitCombination {
  val id = 79
  val points = 1
  val name = "Single Wait"
  val description = "Single wait on pair"
  val fullHand = false

  def matchingWait(figure: Figure, waitingTile: Tile): Boolean = {
    figure match {
      case Dui(tile) if tile == waitingTile => true
      case _ => false
    }
  }

}


object ClosedWait extends Combination {
  val id = 78
  val points = 1
  val name = "Closed Wait"
  val description = "Single wait on the middle of a chow"
  val fullHand = false

  def find(m: HuLe): Option[Figures] = {
    if (m.lastTileContext.origin != Discarded) None
    else {
      val closedTiles = m.closed.map(_.asList).flatten
      val tilesBeforeWinning: TileSet = TileSet(closedTiles).removed(m.lastTileContext.tile)
      val waitingTiles = UniqueWait.waitingTiles(tilesBeforeWinning, m.disclosed)
      println(waitingTiles)

      waitingTiles match {
        case w :: Nil => {
          m.closed.find(_ match {
            case Chow(_, middle, _) if middle == w => true
            case _ => false
          }).map(List(_))
        }
        case _ => None
      }
    }
  }
}

object PungOfTerminalOrHonors extends Combination {
  val id = 73
  val points = 1
  val name = "Pung of Terminals or Honor"
  val description = "Pung of 1 or 9 or winds"
  val fullHand = false

  def find(m: HuLe): Option[Figures] = {
    m.allPungs.find {
      case Pung(Tile(family, value)) if value == 1 || value == 9 || family.isInstanceOf[WindFamily] => true
      case _ => false
    }.map(List(_))
  }
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

object SeatWind extends Combination {
  val id = 61
  val points = 2
  val name = "Seat Wind"
  val description = "1 pung of player's wind"
  val fullHand = false

  override val implied = List(PungOfTerminalOrHonors)

  def find(m: HuLe): Option[Figures] =
    m.allPungs.find(_.t.family == m.context.seatWind) match {
      case None => None
      case Some(pung) => Some(List(pung))
    }
}

object FullyConcealedHand extends Combination {
  val id = 56
  val points = 4
  val name = "Fully Concealed Hand"
  val description = "Nothing disclosed, finish on self drawn"
  val fullHand = true

  override val implied = List(SelfDrawnComb)

  def find(m: HuLe): Option[Figures] =
    m.lastTileContext.origin == SelfDrawn && m.disclosed.size == 0
    match {
      case true => Some(m.allFigures)
      case false => None
    }
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

object AllTypes extends Combination {
  val id = 52
  val points = 6
  val name = "All Types"
  val description = "5 figures in all families (bamboo, character, stone, wind, honor)"
  val fullHand = false

  def find(m: HuLe): Option[Figures] = {

    def hasFamily(family: Family, figures: Figures) =
      figures.exists(figure => figure match {
        case f: Knitted if family.isInstanceOf[SuitFamily] => true
        case f: SomeKnittedWithSomeDragons => true
        case f => f.asList.forall(_.family == family)
      })


    List(List(Bamboo), List(Character), List(Stone),
      List(EastWind, NorthWind, WestWind, SouthWind),
      List(RedDragon, WhiteDragon, GreenDragon))
      .forall(listFamily => listFamily.exists(family => hasFamily(family, m.allFigures)))
    match {
      case true => Some(m.allFigures)
      case false => None
    }

  }
}

object HalfFlush extends Combination {
  val id = 50
  val points = 6
  val name = "Half Flush"
  val description = "all tiles of the same type (bamboos, character or stone) plus some honors "
  val fullHand = true

  def find(m: HuLe): Option[Figures] = {
    val (honors, straight) = m.allTiles.partition(_.family.isInstanceOf[HonorFamily])

    honors.size > 0 &&
      straight.map(_.family).toSet.size == 1
    match {
      case true => Some(m.allFigures)
      case false => None
    }
  }
}


object MixedTripleChow extends Combination {
  val id = 41
  val points = 8
  val name = "Mixed Triple Chow"
  val description = "Three identical chows in three families"
  val fullHand = false

  override val implied = List(MixedDoubleChows)

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

object UpperFour extends Combination {
  val id = 36
  val points = 12
  val name = "Upper Four"
  val description = "Only 9,8,7,6"
  val fullHand = true

  def find(m: HuLe): Option[Figures] = {
    m.allTiles.forall(t => t.family.isInstanceOf[SuitFamily] && t.value >= 6) match {
      case false => None
      case true => Some(m.allFigures)
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

  override val implied = List(AllTypes)

  def find(m: HuLe): Option[Figures] =
    m.closed.find(_.isInstanceOf[SomeKnittedWithSomeDragons]).map(List(_))
}

object GreaterHonorsAndKnittedTiles extends Combination {
  val id = 20
  val points = 24
  val name = "Greater Honors and Knitted Tiles"
  val description = "The 7 unique honors and incomplete knitted tiles"
  val fullHand = true

  override val implied = List(LesserHonorsAndKnittedTiles, AllTypes)

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

  override val implied = List(SingleWait)

  def find(m: HuLe): Option[Figures] =
    if (m.closed.size == 7 && m.closed.forall(_.isInstanceOf[Dui]))
      Some(m.closed)
    else None
}

object AllTerminalsAndHonors extends Combination {
  val id = 18
  val points = 32
  val name = "All Terminals And Honors"
  val description = "only 1, 9 and honors"
  val fullHand = true

  def find(m: HuLe): Option[Figures] = {
    m.allTiles.forall {
      case Tile(family, value) if value == 1 || value == 9 || family.isInstanceOf[HonorFamily] => true
      case _ => false
    }
    match {
      case true => Some(m.allFigures)
      case false => None
    }
  }
}


object ThirteenOrphansComb extends Combination {
  val id = 7
  val points = 88
  val name = "Thirteen Orphans"
  val description = "1 and 9 in three families and 7 distinct honors plus 1 extra honor"
  val fullHand = true

  override val implied = List(AllTerminalsAndHonors)

  def find(m: HuLe): Option[Figures] =
    m.closed.find {
      figure => figure.isInstanceOf[ThirteenOrphans]
    }.map(List(_))
}