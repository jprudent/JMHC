package org.liprudent.majiang

import mahjong._
import mahjong.DetailedPoints
import mahjong.HuLe
import mahjong.PlayerTiles
import org.liprudent.majiang.figures._
import tiles._
import org.liprudent.majiang.tiles.Types._
import org.liprudent.majiang.figures.SomeKnittedWithSomeDragons
import org.liprudent.majiang.figures.ThirteenOrphans
import org.liprudent.majiang.figures.Knitted
import org.liprudent.majiang.figures.Pung
import org.liprudent.majiang.figures.Bonus
import org.liprudent.majiang.figures.Dui
import tiles.ContextualTile

package object mahjong {

  /**
   * Represent the different sets of tiles owned by a player.
   * @param hand The hand
   * @param disclosed The tiles disclosed and contained in figures
   * @param bonus Flowers and seasons. Default is Nil.
   */
  case class PlayerTiles(hand: Hand, disclosed: Figures, bonus: Bonus = Bonus(Nil)) {

    require(disclosed.sorted(OrdFigure) == disclosed, "disclosed not sorted")

    lazy val size = hand.tileSet.size + disclosedSize
    lazy val disclosedSize = disclosed.foldLeft(0)((sum: Int, f: Figure) => sum + f.properties.size)
  }

  /**
   * Context of the game
   * @param seatWind Player's wind
   * @param prevalentWind Game's wind
   */
  case class PlayerContext(seatWind: WindFamily, prevalentWind: WindFamily)

  /**
   * Sets of tiles representing a Hu Le (mahjong hand)
   * @param closed Figures in closed hand
   * @param disclosed disclosed figures
   * @param lastTileContext context of last tile
   * @param context Player's context
   * @param bonus flowers and seasons
   */
  case class HuLe(
                   closed: Figures,
                   disclosed: Figures,
                   lastTileContext: ContextualTile,
                   context: PlayerContext,
                   bonus: Bonus = Bonus(Nil)) {

    require(closed == closed.sorted(OrdFigure), "not sorted")
    require(disclosed == disclosed.sorted(OrdFigure), "not sorted")

    lazy val allFigures = (closed ::: disclosed).sorted(OrdFigure)
    lazy val allPungs: List[Pung] = allFigures.filter(_.isInstanceOf[Pung]).asInstanceOf[List[Pung]]
    lazy val allChows: List[Chow] = allFigures.filter(_.isInstanceOf[Chow]).asInstanceOf[List[Chow]]
    lazy val allDuis: List[Dui] = allFigures.filter(_.isInstanceOf[Dui]).asInstanceOf[List[Dui]]
    lazy val allTiles: List[Tile] = allFigures.map(_.asList).flatten
  }


  case class DetailedPoints(huLe: HuLe, detailedPoints: List[(List[Figure], Combination)]) {

    require(detailedPoints.forall {
      case (figures, comn) => figures.sorted(OrdFigure) == figures
    }, "not sorted")

    override def toString = {
      val title = "For Hule : " + huLe + "\n"
      val detail = detailedPoints.foldLeft("")((string, line) => {
        string + line._2 + ":" + line._1 + "\n"
      })
      title + detail
    }

    lazy val total = detailedPoints.foldLeft(0)((total, c) => {
      val combinationPoints = c._2 match {
        case FlowerTiles => c._1(0).asList.size
        case _ => c._2.points
      }
      combinationPoints + total
    })
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

    val combinations = List[Combination](
      FlowerTiles,
      SelfDrawnComb,
      SingleWait,
      ClosedWait,
      EdgeWait,
      OneVoidedSuit,
      PungOfTerminalOrHonors,
      ShortStraight,
      MixedDoubleChows,
      AllChows,
      SeatWind,
      DragonPung,
      FullyConcealedHand,
      OutsideHand,
      MeldedHand,
      AllTypes,
      MixedShiftedChow,
      HalfFlush,
      MixedTripleChow,
      UpperFour,
      KnittedStraight,
      LesserHonorsAndKnittedTiles,
      GreaterHonorsAndKnittedTiles,
      SevenPairs,
      AllTerminalsAndHonors,
      ThirteenOrphansComb)

    private def findAllMatchingCombinations(hule: HuLe): List[(Combination, Figures)] = {
      Nil
    }

    def apply(huLe: HuLe): DetailedPoints = {
      val res: List[Option[Figures]] = combinations.map(combination => combination.find(huLe))
      val zipped: List[(Option[Figures], Combination)] = res.zip(combinations)
      val allDetailedPoints: List[(Figures, Combination)] = zipped.filter {
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
        case head :: tail => {
          val filteredTail = tail.filterNot {
            case tailElem => isExcluded(head, tailElem)
          }
          head :: applyExclusion(filteredTail)
        }
      }
    }


    protected[mahjong] def isExcluded(ref: (Figures, Combination), toExclude: (Figures, Combination)): Boolean = {
      ref._2.excludes(toExclude._2) || (
        ref._2.imply(toExclude._2) &&
          ref._1.containsSlice(toExclude._1)
        )
    }
  }

}

case class HuLeFinder(ptiles: PlayerTiles, context: PlayerContext) {

  /**
   *
   * @return A list. Each element is a detailed solution for given <code>ptiles</code>.
   */
  lazy val find: List[DetailedPoints] = {
    if (!quickValid) Nil
    else {
      val computer = FiguresComputer(ptiles.hand.tileSet)
      computer.allFiguresCombinations
        .filter(closedCombination => HuLeFinder.isWellFormedMahjong(closedCombination, ptiles.disclosed))
        .map(closedCombination =>
        HulePointsComputer(
          HuLe(closedCombination, ptiles.disclosed, ptiles.hand.lastTileContext, context, ptiles.bonus)))
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
      someKnittedSomeDragons(closed) ||
      thirteenOrphans(closed) ||
      sevenPairs(all)
  }

  def sevenPairs(all: List[Figure]) = {
    all.size == 7 && all.forall(_.isInstanceOf[Dui])
  }

  def thirteenOrphans(closed: List[Figure]): Boolean = {
    closed match {
      case ThirteenOrphans(_) :: Nil => true
      case _ => false
    }
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

  def waitingTiles(closed: TileSet, disclosed: List[Figure]): List[Tile] = {


    def satisfy(tile: Tile): Boolean = {
      val added: TileSet = closed.added(tile)
      val allCombinations = FiguresComputer(added).allFiguresCombinations
      allCombinations.filter(possibleClosed => HuLeFinder.isWellFormedMahjong(possibleClosed, disclosed)).size > 0
    }

    val tilesToTry = Tile.allButBonus.filter(tile => closed.occurence(tile) < 4)
    tilesToTry.filter(satisfy).toList.sorted
  }

}