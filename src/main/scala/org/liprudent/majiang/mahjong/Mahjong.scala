package org.liprudent.majiang

import computer.FindAllAndReduce
import mahjong._
import mahjong.DetailedPoints
import mahjong.HuLe
import mahjong.PlayerContext
import mahjong.PlayerTiles
import org.liprudent.majiang.figures._
import tiles._
import org.liprudent.majiang.tiles.Types._
import tiles.ContextualTile
import tiles.Tile._
import org.liprudent.majiang.figures.Knitted
import org.liprudent.majiang.figures.Kong
import org.liprudent.majiang.figures.Bonus
import org.liprudent.majiang.figures.PungLike
import org.liprudent.majiang.figures.SomeKnittedWithSomeDragons
import org.liprudent.majiang.figures.ThirteenOrphans
import org.liprudent.majiang.figures.Pung
import org.liprudent.majiang.figures.Dui

package object mahjong {

  val noKongs: List[Kong] = Nil

  /**
   * Represents the different sets of tiles owned by a player.
   *
   * @param concealed The hand containing the closed tiles of a player
   * @param melded The tiles melded as figures
   * @param concealedKongs List of hidden kongs. By default empty list
   * @param bonus Flowers and seasons. Default is Nil.
   *
   * @note Melded Kongs are included in `melded`
   *       TODO there are too much parameters.
   */
  case class PlayerTiles(concealed: ConcealedTiles, melded: Figures, concealedKongs: List[Kong] = noKongs,
                         bonus: Bonus = Bonus(Nil)) {

    require(melded.sorted(OrdFigure) == melded, "melded not sorted")

    require(concealed.tileSet.toTiles.contains(concealed.lastTileContext.tile) ||
      concealedKongs.exists(_.tile == concealed.lastTileContext.tile), concealed.lastTileContext.tile + " not in " +
      concealed.tileSet.toTiles + " nor in " + concealedKongs)

    lazy val numberOfTiles = concealed.tileSet.size + disclosedSize + concealedKongsSize

    lazy val disclosedSize = count(melded)

    lazy val concealedKongsSize = count(concealedKongs)

    lazy val numberOfKongs = concealedKongs.size + melded.filter(_.isInstanceOf[Kong]).size

    private def count(figures: List[Figure]) =
      figures.foldLeft(0)((sum: Int, f: Figure) => sum + f.properties.size)
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
   * @param melded melded figures
   * @param lastTileContext context of last tile
   * @param context Player's context
   * @param waitingTiles Other tiles that may lead to a valid mahjong hand
   * @param bonus flowers and seasons
   */
  case class HuLe(closed: Figures, melded: Figures, lastTileContext: ContextualTile, context: PlayerContext,
                  waitingTiles: List[Tile], concealedKongs: List[Kong] = noKongs, bonus: Bonus = Bonus(Nil)) {

    require(closed == closed.sorted(OrdFigure), "not sorted")
    require(melded == melded.sorted(OrdFigure), "not sorted")

    require(melded.forall(_ match {
      case f: Dui => false
      case f: Knitted => false
      case f: SomeKnittedWithSomeDragons => false
      case f: ThirteenOrphans => false
      case _ => true
    }), "melded figures can only be kong, chow or pung")


    /* ANY FIGURES */
    lazy val allFigures = (closed ::: melded ::: concealedKongs).sorted(OrdFigure)

    lazy val allClosedStraightFamilyFigures: List[Figure] =
      allFigures.filter(_.toTiles.forall(_.isStraight))

    lazy val allDragonFigures: List[Figure] =
      allDragonPungsLike ::: allDuis.filter(_.tile.isDragon)

    lazy val allWindFigures: List[Figure] =
      allWindPungsLike ::: allDuis.filter(_.tile.isWind)

    lazy val allButKongs = allFigures.filterNot(_.isInstanceOf[Kong])


    /* KONGS */
    lazy val allMeldedKongs: List[Kong] = melded.filter(_.isInstanceOf[Kong]).asInstanceOf[List[Kong]]

    lazy val allKongs: List[Kong] = (allMeldedKongs ::: concealedKongs).sorted(OrdFigure)


    /* PUNGS & KONGS */
    lazy val allPungsLike: List[PungLike] = allFigures.filter(_.isInstanceOf[PungLike]).asInstanceOf[List[PungLike]]

    lazy val allDragonPungsLike: List[PungLike] = allPungsLike.filter(_.tile.isDragon)

    lazy val allWindPungsLike: List[PungLike] = allPungsLike.filter(p => p.tile.isWind)

    lazy val allStraightPungLike = allPungsLike.filter(_.tile.isStraight)

    lazy val allConcealedPungLike = allPungsLike.filterNot(p => melded.contains(p) ||
      (lastTileContext.origin != SelfDrawn && p.tile == lastTileContext.tile))

    lazy val allPungsLikeOfTerminalOrWind = allPungsLike.filter {
      _.tile.isTerminalOrWind
    }


    /* CHOWS */
    lazy val allChows: List[Chow] = allFigures.filter(_.isInstanceOf[Chow]).asInstanceOf[List[Chow]]


    /* PAIRS */
    lazy val allDuis: List[Dui] = allFigures.filter(_.isInstanceOf[Dui]).asInstanceOf[List[Dui]]


    /* SPECIAL FIGURES */
    lazy val allKnittedTiles = closed.filter(_.isInstanceOf[Knitted])

    lazy val allKnittedWithDragons = closed.filter(_.isInstanceOf[SomeKnittedWithSomeDragons])

    lazy val allThirteenOrphans = closed.filter {
      _.isInstanceOf[ThirteenOrphans]
    }


    /* TILES */
    lazy val allTiles: List[Tile] = allFigures.map(_.toTiles).flatten

    lazy val hasHonors : Boolean = allTiles.exists(_.isHonor)

    lazy val allTileset: TileSet = TileSet(allTiles)

    lazy val allClosedTiles: List[Tile] = closed.map(_.toTiles).flatten

    lazy val numberOfStraightFamily = numberOfStraightFamilyIn(allTiles)

    def numberOfStraightFamilyIn(tiles: List[Tile]): Int = {
      tiles.groupBy(tile => tile.family match {
        case f: HonorFamily => "honors"
        case f => f.name
      }).filter {
        case (family, tiles) if family == "honors" => false
        case _ => true
      }.keySet.size
    }

    /**
     * A standard hand is made of 5 figures. It excludes Knitted, Seven Pairs, ...
     */
    lazy val standardHand = allFigures.size == 5

  }


  case class DetailedPoints(huLe: HuLe, detailedPoints: List[(List[Figure], Combination)]) {

    require(detailedPoints.forall {
      case (figures, comb) => figures.sorted(OrdFigure) == figures
    }, ("not sorted \n" + detailedPoints))

    override def toString = {
      val title = "For Hule : " + huLe + "\n"
      val detail = detailedPoints.foldLeft("")((string, line) => {
        string + line._2 + ":" + line._1 + "\n"
      })
      title + detail
    }

    lazy val total = detailedPoints.foldLeft(0)((total, c) => {
      val combinationPoints = c._2 match {
        case FlowerTiles => c._1(0).toTiles.size
        case _ => c._2.points
      }
      combinationPoints + total
    })

    def hasCombination(combination: Combination): Boolean =
      detailedPoints.exists(_._2 == combination)

    def hasCombination(combinationName: String): Boolean =
      hasCombination(toCombination(combinationName))

    def hasCombinationOnce(combination:Combination): Boolean = {
      detailedPoints.count(_._2 == combination) == 1
    }

    /**
     * Convert a combination name to a domain Combination object
     * @param combinationName case insensitive. The combination name to convert
     * @return The combination or throw an exception if not found
     */
    def toCombination(combinationName: String): Combination = {
      HulePointsComputer.combinations.find(_.name.toUpperCase == combinationName.toUpperCase) match {
        case None => throw new IllegalArgumentException("Unknown combination name : " + combinationName)
        case Some(mappedCombination) => mappedCombination
      }
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

    //TODO move it to Combination
    val combinations = List[Combination](ChickenHand, FlowerTiles, SelfDrawnComb, SingleWait, ClosedWait, EdgeWait,
      NoHonors, OneVoidedSuit, MeldedKong, PungOfTerminalsOrHonors, TwoTerminalChows, ShortStraight,
      MixedDoubleChows, PureDoubleChows, AllSimples, ConcealedKong, TwoConcealedPungs, DoublePung, TileHog, AllChows,
      ConcealedHand, SeatWind, PrevalentWind, DragonPung, AllPungs, TwoConcealedKongs, RobbingTheKong,
      OutWithRemplacementTile, LastTileClaimComb, LastTileDrawComb, LastTile, TwoMeldedKongs, FullyConcealedHand,
      OutsideHand, TwoDragonPungs, MeldedHand, AllTypes, MixedShiftedChow, HalfFlush, MixedShiftedPung,
      MixedTripleChows, ReversibleTiles, MixedStraight, BigThreeWind, LowerFour, UpperFour, KnittedStraight,
      LesserHonorsAndKnittedTiles, ThreeConcealedPungs, TriplePungs, AllFive, PureShiftedChow,
      ThreeSuitedTerminalChows, PureStraight, LowerTiles, MiddleTiles, UpperTiles, PureShiftedPungs, PureTripleChows,
      FullFlush, AllEvenPungs, GreaterHonorsAndKnittedTiles, SevenPairs, AllTerminalsAndHonors, ThreeKongs,
      FourPureShiftedPungs, FourShiftedChows, QuadrupleChows, PureTerminalChows, FourConcealedPungs, AllHonors,
      LittleThreeDragons, LittleFourWinds, AllTerminals, ThirteenOrphansComb, SevenShiftedPairs, FourKongs,
      NineGates, AllGreen, BigThreeDragons, BigFourWinds).reverse

    require(combinations.size == 81)

    //chicken hand is the last one computed
    require(combinations.last == ChickenHand)

    //starts with the biggest
    require(combinations.head.points == 88)

    def apply(huLe: HuLe): DetailedPoints = {

      type DetailedPointType = (Figures, Combination)

      val allDetailedPoints: List[DetailedPointType] = combinations.foldLeft((List[DetailedPointType](),
        Set[Combination]())) {
        (res, combination) =>

          def findCombination = {

            //compute the combination against hule
            val result: Result = combination.find(huLe)

            if (result == EmptyResult) {
              //if result is empty, filters out
              res

            } else {
              //map result to a list of (Figures,Combination)
              val newDetailedPoints = result.figures.map((_, combination)) ::: res._1

              //enhance the exclusion list
              val newExclusionList = res._2 ++ combination.excluded

              (newDetailedPoints, newExclusionList)
            }
          }


          //skip if combination is already excluded
          if (res._2.contains(combination)) {
            res
          } else {
            findCombination
          }

      }._1.sorted(OrdDetailedPoint)

      val filteredDetailedPoints = applyExclusion(allDetailedPoints)

      DetailedPoints(huLe, filteredDetailedPoints)
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
      def mutuallyExcludes() = ref._2.excludes(toExclude._2)

      def implies() = ref._2.implies(toExclude._2)

      def same() = ref._2 == toExclude._2

      def toExcludeContainedInRef() = toExclude._1.forall(fig => ref._1.contains(fig))


      mutuallyExcludes() ||
        ((implies() || same()) && toExcludeContainedInRef())


    }
  }

}

case class HuFinder(ptiles: PlayerTiles, context: PlayerContext) {

  /**
   *
   * @return A list ordered by desc total. Each element is a detailed solution for given <code>ptiles</code>.
   */
  lazy val find: List[DetailedPoints] = {
    if (!quickValid) Nil
    else {
      val computer = FindAllAndReduce(ptiles.concealed.tileSet)

      computer.allFiguresCombinations.foldLeft(List[DetailedPoints]()) {
        (res, concealed) =>
          if (HuFinder.isWellFormedMahjong(concealed, ptiles.melded, ptiles.concealedKongs)) {
            val lastTile = ptiles.concealed.lastTileContext.tile
            val waitingTiles = (lastTile ::
              UniqueWait.waitingTiles(ptiles.concealed.withoutLastTile.tileSet, ptiles.melded,
                ptiles.concealedKongs, List(lastTile))
              ).sorted

            val hule = HuLe(concealed, ptiles.melded, ptiles.concealed.lastTileContext, context, waitingTiles,
              ptiles.concealedKongs, ptiles.bonus)

            HulePointsComputer(hule) :: res
          } else {
            res
          }
      }.sortWith((detail1, detail2) => detail1.total >= detail2.total)
    }
  }

  lazy val quickValid = {
    ptiles.numberOfTiles == 14 + ptiles.numberOfKongs
  }

}

object HuFinder {
  def isWellFormedMahjong(closed: Figures, disclosed: Figures, concealedKongs: List[Kong]): Boolean = {
    val all = closed ::: disclosed ::: concealedKongs
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
    all.size == 5 &&
      all.filter(_ match {
        case f: Kong => true
        case f: Pung => true
        case f: Chow => true
        case _ => false
      }).size == 4 &&
      all.filter(_ match {
        case f: Dui => true
        case _ => false
      }).size == 1
  }
}

object UniqueWait {

  /**
   *
   * @param concealed tiles before winning
   * @param melded figures
   * @param concealedKongs
   * @param exclude A collection of tiles to exclude from result.
   *                Typically if you already computed a valid mahjong, you know that last tile is
   *                a result and you don't want to recompute that.
   *                Default is `Nil`
   * @return the list of waiting tiles
   */
  def waitingTiles(concealed: TileSet, melded: List[Figure], concealedKongs: List[Kong], exclude: Seq[Tile] = Nil): List[Tile] = {


    def satisfy(tile: Tile): Boolean = {
      val added: TileSet = concealed.added(tile)
      val allCombinations = computer.FindAllAndReduce(added).allFiguresCombinations
      allCombinations.filter(possibleClosed => HuFinder.isWellFormedMahjong(possibleClosed, melded,
        concealedKongs)).size > 0
    }

    //all tiles to try. Filter out tiles that has 4 occurence (there can not be 5)
    val tilesToTry = Tile.allButBonus.filterNot(tile =>
      concealed.occurence(tile) >= 4 || exclude.contains(tile)
    )

    tilesToTry.filter(satisfy).toList.sorted
  }

}
