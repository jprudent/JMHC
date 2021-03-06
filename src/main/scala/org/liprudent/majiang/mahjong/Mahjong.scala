package org.liprudent.majiang.mahjong

import org.liprudent.majiang.figures.{Bonus, Dui, Knitted, Kong, PungLike, SomeKnittedWithSomeDragons, ThirteenOrphans, _}
import org.liprudent.majiang.tiles.Types._
import org.liprudent.majiang.tiles.{ContextualTile, _}


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
                waitingTiles: List[Tile], concealedKongs: List[Kong] = Nil, bonus: Bonus = Bonus(Nil)) {

  require(closed == closed.sorted(OrdFigure), "not sorted")
  require(melded == melded.sorted(OrdFigure), "not sorted")

  require(melded.forall(_ match {
    case f: Dui => false
    case f: Knitted => false
    case f: SomeKnittedWithSomeDragons => false
    case f: ThirteenOrphans => false
    case _ => true
  }), "melded figures can only be kong, chow or pung")

  require(14 <= (closed ::: melded ::: concealedKongs).foldLeft(0) { (acc, x) => x.toTiles.size + acc})

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

  lazy val hasHonors: Boolean = allTiles.exists(_.isHonor)

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

  def hasCombinationOnce(combination: Combination): Boolean =
    hasCombinationNthce(combination, 1)

  def hasCombinationTwice(combination: Combination): Boolean =
    hasCombinationNthce(combination, 2)

  private def hasCombinationNthce(combination: Combination, n: Int) =
    detailedPoints.count(_._2 == combination) == n

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

    val filteredDetailedPoints = ExclusionPrinciples.exclude(allDetailedPoints)

    DetailedPoints(huLe, filteredDetailedPoints)
  }


}

/**
 *
 * This object apply all rules that prevents a combination to be accounted.
 * 1. Non identical principle
 * 2. Implication principle
 * 3. Exclusion principle
 *
 * Whereas the first two rules are quite straight forward, the latter has a lot of controversy.
 * Here is the best rule I could ever find on internet: http://www.sloperama.com/mjfaq/mjfaq22.html
 * "Once two or three numerical sets have been combined for a sequential-number-based scoring pattern, any other
 * remaining numerical sets in the hand may be combined only once with an already-scored numerical set,
 * when creating additional two- or three-set sequential-number-based scoring patterns, such as straights or "shifted"
 * scoring patterns."
 *
 *
 */
object ExclusionPrinciples {

  /**
   * @param allDetailedPoints what to filter out.
   * @return detailed points where some combinations has been filtered out by rules
   */
  def exclude(allDetailedPoints: List[(Figures, Combination)]): List[(Figures, Combination)] = {
    exclude(allDetailedPoints, Map().withDefaultValue(0))
  }

  private def exclude(allDetailedPoints: List[(Figures, Combination)], used: Map[Figures, Int]): List[(Figures, Combination)] = {

    allDetailedPoints match {

      case Nil => Nil

      case head :: tail => {

        def filteredTail = tail.filterNot(applyOtherRules(head, _))

        if (isCandidateExclusionRule(head)) {
          if (hasFigureUsedMoreThanTwice(head._1, used)) {
            exclude(tail, used)
          } else {
            head :: exclude(filteredTail, updateUsedFigures(head._1, used))
          }
        } else {
          head :: exclude(filteredTail, used)
        }
      }
    }
  }

  private def isCandidateExclusionRule(points: (Figures, Combination)) = {
    val (figures, combinations) = points
    !figures.exists(_.toTiles.exists(_.isHonor)) &&
      combinations.shifted &&
      !isHandPropertyCombination(figures)
  }

  //TODO narrow visibility to protected or private
  def applyOtherRules(ref: (Figures, Combination), toExclude: (Figures, Combination)): Boolean = {
    def mutuallyExcludes() = ref._2.excludes(toExclude._2)

    def implies() = ref._2.implies(toExclude._2)

    def same() = ref._2 == toExclude._2

    def toExcludeContainedInRef() = toExclude._1.forall(fig => ref._1.contains(fig))


    mutuallyExcludes() ||
      ((implies() || same()) && toExcludeContainedInRef())


  }

  private def updateUsedFigures(figures: Figures, used: Map[Figures, Int]): Map[Figures, Int] = {

    val updatedUsedFigure = used.find {
      case (k, v) => k.intersect(figures).size > 0
    } match {
      case Some((fs, i)) => used.updated(fs, i + 1)
      // in that case, it means it's the initial usage of figures
      case None => used
    }

    updatedUsedFigure.updated(figures, 1)
  }

  private def isHandPropertyCombination(figures: Figures) =
    figures.foldLeft(0)((tot, f) => tot + f.toTiles.size) == 14

  private def hasFigureUsedMoreThanTwice(figures: Figures, used: Map[Figures, Int]) = {
    used.find {
      case (k, v) => k.intersect(figures).size > 0
    }
    match {
      case Some((fs, i)) if i >= 2 => true
      case _ => false
    }
  }
}







