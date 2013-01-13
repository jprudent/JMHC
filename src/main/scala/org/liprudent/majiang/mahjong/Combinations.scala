package org.liprudent.majiang.mahjong

import org.liprudent.majiang.figures._
import org.liprudent.majiang.tiles.Types.Figures
import org.liprudent.majiang.tiles._
import org.liprudent.majiang.tiles.Tile._
import org.liprudent.majiang.UniqueWait
import org.liprudent.majiang.figures.ThirteenOrphans
import org.liprudent.majiang.figures.Knitted
import org.liprudent.majiang.figures.Dui
import scala.Some
import org.liprudent.majiang.figures.SomeKnittedWithSomeDragons

sealed trait Result {
  val figures: List[Figures]
}

object Result {

  def apply(figure: Figure) = Fan(List(List(figure)))

  def apply(optFigure: Option[Figure]) =
    optFigure match {
      case Some(figure) => Fan(List(List(figure)))
      case None => EmptyResult
    }

  def apply(figures: Figures) =
    figures match {
      case Nil => EmptyResult
      case _ => Fan(List(figures))
    }

  //NOTE : The implicit parameter is here to bypass type erasure problems (same signature as above)
  def apply(multipleMatch: List[Figures])(implicit dummy: DummyImplicit) =
    multipleMatch match {
      case Nil => EmptyResult
      case _ => Fan(multipleMatch)
    }

}

object SomeResult {

  def apply(figure: Figure)(cond: Boolean): Result =
    apply(List(List(figure)))(cond)

  def apply(figures: Figures)(cond: Boolean): Result =
    apply(List(figures))(cond)

  def apply(figures: List[Figures])(cond: Boolean)(implicit dummy: DummyImplicit): Result = {
    cond match {
      case true => Result(figures)
      case false => EmptyResult
    }
  }
}

object EmptyResult extends Result {
  val figures = Nil
}

case class Fan(figures: List[Figures]) extends Result


sealed trait Combination {
  val id: Int
  val points: Int
  val name: String
  val description: String

  def find(m: HuLe): Result

  /**
   * List of implied combinations by this one.
   * A combination implies another based on the same figures or a subset of figures
   * example: "Seat Wind" implies "Pung of honor or extremity" if it's a pung of Wind West
   * if it's a Pung of Wind West and a Pung of 9 bamboos this is not an implication
   */
  val implied = List[Combination]()

  /**
   * A list of mutually excluded combinations. If a combination excludes another, they cannot be scored both.
   * Example: When scoring, Wait on the Pair and Edge Wait are mutually excluded
   */
  val excluded = List[Combination]()

  /**
   * Check wether this combination implies y recursively.
   *
   * @param y the other combination
   * @return true if y is implied by this combination. false otherwise
   * @see [[org.liprudent.majiang.mahjong.Combination# e x c l u d e s ( o r g.l i p r u d e n t.m a j i a n g.m a h j o n g.C o m b i n a t i o n )]]
   */
  def implies(y: Combination): Boolean =
    implied.exists(_ == y) || implied.exists(_.implies(y))

  /**
   * Check wether this combination excludes y recursively
   *
   * @param y the other combination
   * @return true if y is implied by this combination. false otherwise
   * @see [[org.liprudent.majiang.mahjong.Combination# i m p l i e s ( o r g.l i p r u d e n t.m a j i a n g.m a h j o n g.C o m b i n a t i o n )]]
   */
  def excludes(y: Combination): Boolean =
    excluded.exists(_ == y) || excluded.exists(_.excludes(y))

  override lazy val toString = "n?%d, %d points, %s".format(id, points, name)
}

object Combination {

  def findTwoFigures[FigureType <: Figure](figures: List[FigureType])
                                          (cond1: (FigureType) => Boolean)
                                          (cond2: (FigureType, FigureType) => Boolean): List[List[FigureType]] = {
    for {
      f1 <- figures if cond1(f1)
      f2 <- figures.dropWhile(_ != f1).tail if cond2(f1, f2)
    } yield (List(f1, f2).sorted(OrdFigure))

  }

  def findThreeFigures[FigureType <: Figure]
  (figures: List[FigureType])
  (cond1: (FigureType) => Boolean)
  (cond2: (FigureType, FigureType) => Boolean)
  (cond3: (FigureType, FigureType, FigureType) => Boolean): List[List[FigureType]] = {
    for {
      f1 <- figures if cond1(f1)
      f2 <- figures if cond2(f1, f2)
      f3 <- figures if cond3(f1, f2, f3)
    } yield (List(f1, f2, f3).sorted(OrdFigure))

  }

  def findTwoTerminalChowsInSameFamily(chows: List[Chow]) =
    findTwoFigures(chows)(_.isStartingChow) {
      (c1, c2) => c2.sameFamily(c1) && c2.isEndingChow
    }

  def findShortStraights(chows: List[Chow]) =
    findTwoFigures(chows)(_ => true) {
      (c1, c2) => c2.sameFamily(c1) && c1.isConsequitive(c2)
    }

  def findMixedDoubleChows(chows: List[Chow]) =
    findTwoFigures(chows)(_ => true) {
      (c1, c2) => c1.family < c2.family && c1.sameValues(c2)
    }

  def findPureDoubleChows(chows: List[Chow]) =
    findTwoFigures(chows)(_ => true) {
      (c1, c2) => c1 == c2
    }

  def findMixedDoublePung(pungs: List[PungLike]) =
    findTwoFigures(pungs)(_ => true) {
      (p1, p2) => !p1.sameFamily(p2) && p1.sameValue(p2)
    }

  def findMixedShiftedChow(chows: List[Chow]) =
    findThreeFigures(chows)(_ => true) {
      (c1, c2) => c2.t1.value == c1.t1.value + 1 && !c1.sameFamily(c2)
    } {
      (c1, c2, c3) => c3.t1.value == c2.t1.value + 1 && !c1.sameFamily(c3) && !c2.sameFamily(c3)
    }

  def findMixedTripleChow(chows: List[Chow]) =
    findThreeFigures(chows)(_ => true) {
      (c1, c2) => c2.sameValues(c1) && c1.family < c2.family
    } {
      (c1, c2, c3) => c3.sameValues(c2) && c1.family < c3.family && c2.family < c3.family
    }

  def findMixedShiftedPungs(pungs: List[PungLike]) = {
    def cond(p1: PungLike, p2: PungLike) =
      p1.tile.family != p2.tile.family && p2.tile.value == p1.tile.value + 1
    findThreeFigures(pungs)(_ => true)((c1, c2) => cond(c1, c2))((c1, c2, c3) => cond(c2, c3))
  }

  def findMixedStraight(chows: List[Chow]) =
    findThreeFigures(chows)(_.isStartingChow) {
      (c1, c2) => c2.isMiddleChow && !c1.sameFamily(c2)
    } {
      (c1, c2, c3) => c3.isEndingChow && !c1.sameFamily(c3) && !c2.sameFamily(c3)
    }

  def findTriplePungs(pungs: List[PungLike]) = {
    def cond(p1: PungLike, p2: PungLike) =
      p1.tile < p2.tile && p2.tile.sameValue(p1.tile)
    findThreeFigures(pungs)(_ => true)((c1, c2) => cond(c1, c2))((c1, c2, c3) => cond(c2, c3))
  }

  def findPureShiftedChow(chows: List[Chow]) =
    findThreeFigures(chows)(_ => true) {
      (c1, c2) => c2.sameFamily(c1) && (c2.t1.value == c1.t1.value + 1 || c2.t1.value == c1.t1.value + 2)
    } {
      (c1, c2, c3) => c3.sameFamily(c1, c2) && (c3.t1.value == c2.t1.value + (c2.t1.value - c1.t1.value))
    }

  def findPureStraight(chows: List[Chow]) =
    findThreeFigures(chows)(_.isStartingChow) {
      (c1, c2) => c2.sameFamily(c1) && c2.isMiddleChow
    } {
      (c1, c2, c3) => c3.sameFamily(c1, c2) && c3.isEndingChow
    }

  def findPureShiftedPungs(pungs: List[PungLike]) = {
    findThreeFigures(pungs) {
      _.tile.isStraight
    } {
      (p1, p2) => p2.sameFamily(p1) && p2.tile.value == p1.tile.value + 1
    } {
      (p1, p2, p3) => p3.sameFamily(p1, p2) && p3.tile.value == p2.tile.value + 1
    }
  }


}

object FlowerTiles extends Combination {
  val id = 81
  val points = 1
  val name = "Flower Tiles"
  val description = "Flowers and Seasons Tiles"


  def find(m: HuLe): Result = {
    m.bonus.bonus match {
      case Nil => EmptyResult
      // TODO 1 tile = 1 match
      case _ => Result(m.bonus)
    }
  }

}

//TODO should return SingleTile
object SelfDrawnComb extends Combination {
  val id = 80
  val points = 1
  val name = "Self Drawn"
  val description = "finish with self drawing"


  override def find(m: HuLe): Result = {
    m.lastTileContext.origin == SelfDrawn match {
      case true => {

        m.closed.find(_.toTiles.contains(m.lastTileContext.tile)) match {
          case None => EmptyResult
          case Some(f) => Result(f)
        }
      }

      case false => EmptyResult
    }
  }

}

sealed trait WaitCombination extends Combination {

  def matchingWait(figure: Figure, waitingTile: Tile): Boolean

  def find(m: HuLe): Result = {

    val tilesBeforeWinning: TileSet = TileSet(m.allClosedTiles).removed(m.lastTileContext.tile)
    val waitingTiles = UniqueWait.waitingTiles(tilesBeforeWinning, m.melded, m.concealedKongs)

    waitingTiles match {

      //a single waiting tile
      case w :: Nil => {
        val waitingFigures = m.closed.filter(figure => matchingWait(figure, w))
        Result(waitingFigures)
      }

      case _ => EmptyResult
    }
  }
}

object SingleWait extends Combination with WaitCombination {
  val id = 79
  val points = 1
  val name = "Single Wait"
  val description = "Single wait on pair"


  def matchingWait(figure: Figure, waitingTile: Tile): Boolean = {
    figure match {
      case Dui(tile) if tile == waitingTile => true
      case _ => false
    }
  }

}


object ClosedWait extends Combination with WaitCombination {
  val id = 78
  val points = 1
  val name = "Closed Wait"
  val description = "Single wait on the middle of a chow"


  override val excluded = List(SingleWait)

  def matchingWait(figure: Figure, waitingTile: Tile): Boolean = {
    figure match {
      case Chow(_, middle, _) if middle == waitingTile => true
      case _ => false
    }
  }

}

object EdgeWait extends Combination with WaitCombination {
  val id = 77
  val points = 1
  val name = "Edge Wait"
  val description = "Single wait on 3 or 7 of a chow"

  override val excluded = List(SingleWait)

  def matchingWait(figure: Figure, waitingTile: Tile): Boolean = {
    figure match {
      case Chow(Tile(f1, 1), middle, last) if last == waitingTile => true
      case Chow(first, middle, Tile(family, 9)) if first == waitingTile => true
      case _ => false
    }
  }

}

object NoHonors extends Combination {
  val id = 76
  val points = 1
  val name = "No Honors"
  val description = "No dragons nor winds"

  def find(m: HuLe): Result = {
    if (m.allTiles.exists(_.isHonor)) EmptyResult
    else Result(m.allFigures)
  }

}


object OneVoidedSuit extends Combination {
  val id = 75
  val points = 1
  val name = "One voided suit"
  val description = "1 family (bamboo, stone, characted) is absent"


  def find(m: HuLe): Result =
    SomeResult(m.allClosedStraightFamilyFigures) {
      m.numberOfStraightFamily == 2
    }
}

object MeldedKong extends Combination {
  val id = 74
  val points = 1
  val name = "Melded Kong"
  val description = "One melded kong"


  def find(m: HuLe): Result =
    SomeResult(m.allMeldedKongs) {
      m.allMeldedKongs.size == 1
    }
}


object PungOfTerminalsOrHonors extends Combination {
  val id = 73
  val points = 1
  val name = "Pung Of Terminals or Honors"
  val description = "Pung of 1 or 9 or winds"


  def find(m: HuLe): Result =

  //each pung is scored
    Result(m.allPungsLikeOfTerminalOrWind.map(List(_)))


}

object TwoTerminalChows extends Combination {
  val id = 72
  val points = 1
  val name = "Two terminal chows"
  val description = "1, 2, 3 and 7, 8, 9 in one family"

  def find(m: HuLe): Result =
    Result(Combination.findTwoTerminalChowsInSameFamily(m.allChows))
}

object ShortStraight extends Combination {
  val id = 71
  val points = 1
  val name = "Short Straight"
  val description = "2 consequitives chows in same family"

  def find(m: HuLe): Result =
  //all short straight is scored
    Result(Combination.findShortStraights(m.allChows))

}

object MixedDoubleChows extends Combination {
  val id = 70
  val points = 1
  val name = "Mixed Double Chows"
  val description = "2 identical chows in two families"

  def find(m: HuLe): Result =
  //all mixed double chow is scored
    Result(Combination.findMixedDoubleChows(m.allChows))

}

object PureDoubleChows extends Combination {
  val id = 69
  val points = 1
  val name = "Pure Double Chows"
  val description = "Two identical chows in the same family"

  def find(m: HuLe): Result = {
    //all pure double chow is scored
    Result(Combination.findPureDoubleChows(m.allChows))
  }

}

object AllSimples extends Combination {
  val id = 68
  val points = 2
  val name = "All Simples"
  val description = "Only 2, 3, 4, 5, 6, 7, 8"

  override val excluded = List(NoHonors)

  def find(m: HuLe): Result =
    SomeResult(m.allFigures) {
      !m.allTiles.exists(_.isTerminalOrHonor)
    }
}

object ConcealedKong extends Combination {
  val id = 67
  val points = 2
  val name = "Concealed Kong"
  val description = "One concealed kong"

  def find(m: HuLe): Result =
    SomeResult(m.concealedKongs) {
      m.concealedKongs.size == 1
    }

}

object TwoConcealedPungs extends Combination {
  val id = 66
  val points = 2
  val name = "Two concealed pungs"
  val description = "Two concealed pungs or kongs"

  def find(m: HuLe): Result =
    SomeResult(m.allConcealedPungLike) {
      m.allConcealedPungLike.size == 2
    }
}


object DoublePung extends Combination {
  val id = 65
  val points = 2
  val name = "Double Pung"
  val description = "Two identical pung in two families"

  def find(m: HuLe): Result =
    Result(Combination.findMixedDoublePung(m.allStraightPungLike))

}

object TileHog extends Combination {
  val id = 64
  val points = 2
  val name = "Tile Hog"
  val description = "4 identical tiles in at least 2 figures"

  def find(m: HuLe): Result = {
    val allFiguresButKong = m.allFigures.filterNot(_.isInstanceOf[Kong])
    val allTilesButKong = allFiguresButKong.map(f => f.toTiles).flatten
    val allTileHogs = TileSet(allTilesButKong).tocs.filter {
      case (tile, occurence) if occurence == 4 => true
      case _ => false
    }.map {
      case (tile, occurence) => allFiguresButKong.filter(_.toTiles.contains(tile))
    }

    Result(allTileHogs)
  }
}

object AllChows extends Combination {
  val id = 63
  val points = 2
  val name = "All Chows"
  val description = "All Chows but one Pair"

  override val excluded = List(NoHonors)

  def find(m: HuLe): Result =
    SomeResult(m.allChows) {
      m.allDuis.size == 1 && m.allChows.size == 4
    }
}

object ConcealedHand extends Combination {
  val id = 62
  val points = 2
  val name = "Concealed Hand"
  val description = "Nothing melded but finish on a discard"

  def find(m: HuLe): Result =
    SomeResult(m.allFigures) {
      m.melded == Nil && m.lastTileContext.origin == Discarded
    }
}

object SeatWind extends Combination {
  val id = 61
  val points = 2
  val name = "Seat Wind"
  val description = "1 pung of player's wind"

  override val implied = List(PungOfTerminalsOrHonors)

  def find(m: HuLe): Result =
    Result(m.allPungsLike.find(_.tile.family == m.context.seatWind))
}

object PrevalentWind extends Combination {
  val id = 60
  val points = 2
  val name = "Prevalent Wind"
  val description = "1 pung of round wind"

  override val implied = List(PungOfTerminalsOrHonors)

  def find(m: HuLe): Result =
    Result(m.allPungsLike.find(_.tile.family == m.context.prevalentWind))
}

object DragonPung extends Combination {
  val id = 59
  val points = 2
  val name = "Dragon Pung"
  val description = "A pung of dragon"

  def find(m: HuLe): Result =
  // all dragon pung is scored
    Result(m.allDragonPungsLike.map(List(_)))
}

object LastTile extends Combination {
  val id = 58
  val points = 4
  val name = "Last Tile"
  val description = "Finish with the fourth tile when all other three are visible"

  def find(m: HuLe): Result =
    SomeResult(SingleTile(m.lastTileContext.tile)) {
      m.lastTileContext.lastTileSituation == LastTileOfKind
    }
}

object TwoMeldedKongs extends Combination {
  val id = 57
  val points = 4
  val name = "Two melded kongs"
  val description = "Two melded kongs or one melded and one concealed kongs"

  override val excluded = List(MeldedKong)

  def find(m: HuLe): Result =
    SomeResult(m.allKongs) {
      m.allMeldedKongs.size == 2 || (m.allMeldedKongs.size == 1 && m.concealedKongs.size == 1)
    }

}

object FullyConcealedHand extends Combination {
  val id = 56
  val points = 4
  val name = "Fully Concealed Hand"
  val description = "Nothing melded, finish on self drawn"


  override val implied = List(SelfDrawnComb)

  def find(m: HuLe): Result =
    SomeResult(m.allFigures) {
      m.lastTileContext.origin == SelfDrawn && m.melded.size == 0
    }
}

object OutsideHand extends Combination {
  val id = 55
  val points = 4
  val name = "Outside Hand"
  val description = "At least 1 honor or 1 terminal in each figure"

  def find(m: HuLe): Result =
    SomeResult(m.allFigures) {
      m.allFigures.forall(figure => figure match {
        case f: SomeKnittedWithSomeDragons => false
        case f: Knitted => false
        case f: ThirteenOrphans => false
        case f => f.toTiles.exists(_.isTerminalOrHonor)
      })
    }
}

object TwoDragonPungs extends Combination {
  val id = 54
  val points = 6
  val name = "Two dragon pungs"
  val description = "Two dragon pungs"

  override val excluded = List(DragonPung)

  def find(m: HuLe): Result =
    SomeResult(m.allDragonPungsLike) {
      m.allDragonPungsLike.size == 2
    }
}

object MeldedHand extends Combination {
  val id = 53
  val points = 6
  val name = "Melded Hand"
  val description = "4 figures melded, and finish on discard"

  override val excluded = List(SingleWait)

  def find(m: HuLe): Result =
    SomeResult(m.allFigures) {
      m.melded.size == 4 && m.lastTileContext.origin != SelfDrawn
    }

}

object AllTypes extends Combination {
  val id = 52
  val points = 6
  val name = "All Types"
  val description = "5 figures in all families (bamboo, character, stone, wind, honor)"

  def find(m: HuLe): Result =
    SomeResult(m.allFigures) {
      def hasFamily(family: Family, figures: Figures) =
        figures.exists(figure => figure match {
          case f: Knitted if family.isInstanceOf[StraightFamily] => true
          case f: SomeKnittedWithSomeDragons => true
          case f => f.toTiles.forall(_.family == family)
        })

      List(List(Bamboo), List(Character), List(Stone),
        List(EastWind, NorthWind, WestWind, SouthWind),
        List(RedDragon, WhiteDragon, GreenDragon))
        .forall(listFamily => listFamily.exists(family => hasFamily(family, m.allFigures)))

    }

}

object MixedShiftedChow extends Combination {
  val id = 51
  val points = 6
  val name = "Mixed Shifted Chow"
  val description = "3 chows in 3 families shifted by 2 tiles"

  def find(m: HuLe): Result =
  //Every mixed shifted chow is scored
    Result(Combination.findMixedShiftedChow(m.allChows))

}

object HalfFlush extends Combination {
  val id = 50
  val points = 6
  val name = "Half Flush"
  val description = "all tiles of the same type (bamboos, character or stone) plus some honors "


  def find(m: HuLe): Result =
    SomeResult(m.allFigures) {

      val (honors, straight) = m.allTiles.partition(_.family.isInstanceOf[HonorFamily])

      honors.size > 0 &&
        straight.map(_.family).toSet.size == 1
    }
}

object AllPungs extends Combination {
  val id = 49
  val points = 6
  val name = "All Pungs"
  val description = "All Pungs and one pair"

  def find(m: HuLe): Result =
    SomeResult(m.allPungsLike) {
      m.allPungsLike.size == 4
    }

}

object TwoConcealedKongs extends Combination {
  val id = 48
  val points = 8
  val name = "Two concealed kongs"
  val description = "Two concealed kong"

  override val excluded = List(TwoConcealedPungs)

  def find(m: HuLe): Result = {
    SomeResult(m.concealedKongs) {
      m.concealedKongs.size == 2
    }
  }

}

object RobbingTheKong extends Combination {
  val id = 47
  val points = 8
  val name = "Robbing the kong"
  val description = "Finish by robbing the tile of another player which transform a pung to a kong"

  def find(m: HuLe): Result = {
    SomeResult(SingleTile(m.lastTileContext.tile)) {
      m.lastTileContext.origin == KongRobbed
    }
  }

}

object OutWithRemplacementTile extends Combination {
  val id = 46
  val points = 8
  val name = "Out with replacement tile"
  val description = "Finish with the tile that replace the kong tile"

  def find(m: HuLe): Result =
    SomeResult(SingleTile(m.lastTileContext.tile)) {
      m.lastTileContext.origin == ReplacedTile
    }
}

object LastTileClaimComb extends Combination {
  val id = 45
  val points = 8
  val name = "Last tile claim"
  val description = "Finish with a claim of the last discarded tile of the game"

  def find(m: HuLe): Result =
    SomeResult(SingleTile(m.lastTileContext.tile)) {
      m.lastTileContext.lastTileSituation == LastTileClaim
    }
}

object LastTileDrawComb extends Combination {
  val id = 44
  val points = 8
  val name = "Last tile draw"
  val description = "Finish with a claim of the last discarded tile of the game"

  override val excluded = List(SelfDrawnComb)

  def find(m: HuLe): Result =
    SomeResult(SingleTile(m.lastTileContext.tile)) {
      m.lastTileContext.lastTileSituation == LastTileDraw
    }
}

object ChickenHand extends Combination {
  val id = 43
  val points = 8
  val name = "Chicken Hand"
  val description = "0 point hand, except flowers"

  override val excluded = List(SelfDrawnComb)

  def find(m: HuLe): Result =
    SomeResult(m.allFigures) {
      val allForbidenThings: List[Combination] = List(
        ThirteenOrphansComb,
        SevenPairs,
        LesserHonorsAndKnittedTiles,
        KnittedStraight,
        MixedStraight,
        ReversibleTiles,
        MixedShiftedPung,
        LastTileDrawComb,
        LastTileClaimComb,
        OutWithRemplacementTile,
        RobbingTheKong,
        AllPungs,
        HalfFlush,
        MixedShiftedChow,
        AllTypes,
        MeldedHand,
        OutsideHand,
        FullyConcealedHand,
        LastTile,
        DragonPung,
        ConcealedHand,
        AllChows,
        TileHog,
        DoublePung,
        TwoConcealedPungs,
        ConcealedKong,
        AllSimples,
        PureDoubleChows,
        MixedDoubleChows,
        ShortStraight,
        TwoTerminalChows,
        PungOfTerminalsOrHonors,
        MeldedKong,
        OneVoidedSuit,
        NoHonors,
        EdgeWait,
        ClosedWait,
        SingleWait,
        SelfDrawnComb)

      allForbidenThings.forall(_.find(m) == EmptyResult)

    }
}


object MixedShiftedPung extends Combination {
  val id = 42
  val points = 8
  val name = "Mixed Shifted Pung"
  val description = "Three consecutive pungs in three families"

  def find(m: HuLe): Result =
    Result(Combination.findMixedShiftedPungs(m.allStraightPungLike))

}

object MixedTripleChows extends Combination {
  val id = 41
  val points = 8
  val name = "Mixed Triple Chows"
  val description = "Three identical chows in three families"


  override val implied = List(MixedDoubleChows)

  def find(m: HuLe): Result =
    Result(Combination.findMixedTripleChow(m.allChows))
}

object ReversibleTiles extends Combination {
  val id = 40
  val points = 8
  val name = "Reversible Tiles"
  val description = "Only 1,2,3,4,5,8,9 stone, 2,4,5,6,8,9 Bamboos, White Dragon"


  override val excluded = List(OneVoidedSuit)

  def find(m: HuLe): Result =
    SomeResult(m.allFigures) {
      val allowedTiles = List(
        s1, s2, s3, s4, s5, s6, s7, s8, s9,
        b2, b4, b5, b6, b8, b9,
        dw
      )

      m.allTiles.forall(allowedTiles.contains(_))
    }
}


object MixedStraight extends Combination {
  val id = 39
  val points = 8
  val name = "Mixed Straight"
  val description = "Three consecutive chows in three family"

  def find(m: HuLe): Result =
    Result(Combination.findMixedStraight(m.allChows))
}

object BigThreeWind extends Combination {
  val id = 38
  val points = 12
  val name = "Big Three Winds"
  val description = "3 pungs of different wind"

  override val implied = List(PungOfTerminalsOrHonors)

  def find(m: HuLe): Result =
    SomeResult(m.allWindPungsLike) {
      m.allWindPungsLike.size == 3
    }
}

object LowerFour extends Combination {
  val id = 37
  val points = 12
  val name = "Lower Four"
  val description = "Only 1, 2, 3, 4"

  override val excluded = List(NoHonors)

  def find(m: HuLe): Result =
    SomeResult(m.allFigures) {
      m.allTiles.forall(t => t.family.isInstanceOf[StraightFamily] && t.value <= 4)
    }
}

object UpperFour extends Combination {
  val id = 36
  val points = 12
  val name = "Upper Four"
  val description = "Only 9, 8, 7, 6"

  override val excluded = List(NoHonors)

  def find(m: HuLe): Result =
    SomeResult(m.allFigures) {
      m.allTiles.forall(t => t.family.isInstanceOf[StraightFamily] && t.value >= 6)
    }
}

object KnittedStraight extends Combination {
  val id = 35
  val points = 12
  val name = "Knitted Straight"
  val description = "1-4-7 in one family, 2-5-8 in the second family, 3-6-9 in the third family"

  def find(m: HuLe): Result =
    Result(m.allKnittedTiles)

}

object LesserHonorsAndKnittedTiles extends Combination {
  val id = 34
  val points = 12
  val name = "Lesser Honors and Knitted Tiles"
  val description = "6 unique honors and incomplete knitted straight OR 5 unique honors and complete knitted straight"

  override val excluded = List(AllTypes, ConcealedHand)

  def find(m: HuLe): Result =
    Result(m.allKnittedWithDragons)

}

object ThreeConcealedPungs extends Combination {
  val id = 33
  val points = 16
  val name = "Three concealed pungs"
  val description = "Three concealed pungs"

  override val excluded = List(TwoConcealedPungs)

  def find(m: HuLe): Result =
    SomeResult(m.allConcealedPungLike) {
      m.allConcealedPungLike.size == 3
    }
}

object TriplePungs extends Combination {
  val id = 32
  val points = 16
  val name = "Triple Pungs"
  val description = "Three identical pungs in three family"

  override val excluded = List(DoublePung)

  def find(m: HuLe): Result =
    Result(Combination.findTriplePungs(m.allStraightPungLike))
}

object AllFive extends Combination {
  val id = 31
  val points = 16
  val name = "All Five"
  val description = "5s in each figures"

  override val excluded = List(AllSimples)

  def find(m: HuLe): Result =
    SomeResult(m.allFigures) {
      m.standardHand && m.allFigures.forall(_.toTiles.exists(_.value == 5))
    }
}


object PureShiftedChow extends Combination {
  val id = 30
  val points = 16
  val name = "Pure shifted chows"
  val description = "3 chows shifted by one or two tiles in the same family"

  def find(m: HuLe): Result = {
    Result(Combination.findPureShiftedChow(m.allChows))
  }
}

object ThreeSuitedTerminalChows extends Combination {
  val id = 29
  val points = 16
  val name = "Three Suited Terminal Chows"
  val description = "1,2,3,7,8,9 in two family and a pair of 5 in the third"

  override val excluded = List(AllChows, MixedDoubleChows, PureDoubleChows, TwoTerminalChows)

  def find(m: HuLe): Result =

    SomeResult(m.allFigures) {
      m.allPungsLike == Nil && m.standardHand && {

        val hasPairOf5InThirdFamily =
          m.allDuis.exists(d => d.tile.value == 5 && !m.allChows.exists(_.t1.sameFamily(d.tile)))

        Combination.findTwoTerminalChowsInSameFamily(m.allChows).size == 2 && hasPairOf5InThirdFamily
      }

    }
}

object PureStraight extends Combination {
  val id = 28
  val points = 16
  val name = "Pure Straight"
  val description = "1-2-3, 4-5-6, 7-8-9 in the same family"

  override val excluded = List(ShortStraight, TwoTerminalChows)

  def find(m: HuLe): Result =
    Result(Combination.findPureStraight(m.allChows))

}

object LowerTiles extends Combination {
  val id = 27
  val points = 24
  val name = "Lower Tiles"
  val description = "Only 1-2-3"

  override val excluded = List(LowerFour)

  def find(m: HuLe): Result =
    SomeResult(m.allFigures) {
      m.allTiles.forall(t => t.isStraight && t.value <= 3)
    }
}

object MiddleTiles extends Combination {
  val id = 27
  val points = 24
  val name = "Middle Tiles"
  val description = "Only 4-5-6"

  override val excluded = List(AllSimples)

  def find(m: HuLe): Result =
    SomeResult(m.allFigures) {
      m.allTiles.forall(t => t.isStraight && t.value >= 4 && t.value <= 6)
    }
}

object UpperTiles extends Combination {
  val id = 25
  val points = 24
  val name = "Upper Tiles"
  val description = "Only 7-8-9"

  override val excluded = List(UpperFour)

  def find(m: HuLe): Result =
    SomeResult(m.allFigures) {
      m.allTiles.forall(t => t.isStraight && t.value >= 7)
    }
}

object PureShiftedPungs extends Combination {
  val id = 24
  val points = 24
  val name = "Pure Shifted Pungs"
  val description = "Three consecutive pungs in the same family"

  override val excluded = List(UpperFour)

  def find(m: HuLe): Result =
    Result(Combination.findPureShiftedPungs(m.allStraightPungLike))
}


object PureTripleChows extends Combination {
  val id = 23
  val points = 24
  val name = "Pure Triple Chows"
  val description = "Three identical chows"

  override val excluded = List(PureDoubleChows)

  def find(m: HuLe): Result = {
    val allPureTripleChows = m.allChows.groupBy(c => c).filter {
      case (chow, chows) => chows.size == 3
    }.values.toList

    Result(allPureTripleChows)
  }
}


object FullFlush extends Combination {
  val id = 22
  val points = 24
  val name = "Full Flush"
  val description = "Only tiles of the same straight family"

  override val excluded = List(NoHonors)

  def find(m: HuLe): Result =
    SomeResult(m.allFigures) {
      m.allTiles.groupBy(tile => tile.family).size == 1
    }
}

object AllEvenPungs extends Combination {
  val id = 21
  val points = 24
  val name = "All Even Pungs"
  val description = "Only tiles 2, 4, 6, 8"

  override val excluded = List(AllPungs, AllSimples)

  def find(m: HuLe): Result =
    SomeResult(m.allFigures) {
      m.allTiles.forall(_.value % 2 == 0)
    }

}

object GreaterHonorsAndKnittedTiles extends Combination {
  val id = 20
  val points = 24
  val name = "Greater Honors and Knitted Tiles"
  val description = "The 7 unique honors and incomplete knitted tiles"

  override val excluded = List(LesserHonorsAndKnittedTiles, AllTypes, ConcealedHand)

  def find(m: HuLe): Result = {
    val allKnittedWith7Dragons = m.closed.filter(figure =>
      figure.isInstanceOf[SomeKnittedWithSomeDragons] && figure.asInstanceOf[SomeKnittedWithSomeDragons].honors.size == 7
    )
    Result(allKnittedWith7Dragons)
  }

}

object SevenPairs extends Combination {
  val id = 19
  val points = 24
  val name = "Seven Pairs"
  val description = "7 pairs"

  override val excluded = List(ConcealedHand, SingleWait)

  def find(m: HuLe): Result =
    SomeResult(m.closed) {
      m.closed.size == 7 && m.closed.forall(_.isInstanceOf[Dui])
    }

}

object AllTerminalsAndHonors extends Combination {
  val id = 18
  val points = 32
  val name = "All Terminals And Honors"
  val description = "only 1, 9 and honors"

  override val excluded = List(OutsideHand)

  def find(m: HuLe): Result =
    SomeResult(m.allFigures) {
      m.allTiles.forall(_.isTerminalOrHonor)
    }

}

object ThreeKongs extends Combination {
  val id = 17
  val points = 32
  val name = "Three Kongs"
  val description = "Three Kongs"

  def find(m: HuLe): Result =
    SomeResult(m.allKongs) {
      m.allKongs.size == 3
    }

}

object FourShiftedChows extends Combination {
  val id = 16
  val points = 32
  val name = "Quadruple Chows"
  val description = "Four shifted chows by 1 or 2 tiles in the same family"

  override val excluded = List(PureShiftedChow, AllChows)

  def find(m: HuLe): Result =
    SomeResult(m.allChows) {
      Combination.findPureShiftedChow(m.allChows).toSet.size == 2
    }

}

object FourPureShiftedPungs extends Combination {
  val id = 15
  val points = 48
  val name = "Four Pure Shifted Pungs"
  val description = "Four pure consecutive pungs in one family"

  override val excluded = List(PureShiftedPungs, AllPungs)

  def find(m: HuLe): Result =
    SomeResult(m.allPungsLike) {
      Combination.findPureShiftedPungs(m.allPungsLike).toSet.size == 2
    }

}

object QuadrupleChows extends Combination {
  val id = 14
  val points = 48
  val name = "Quadruple Chows"
  val description = "Four identical chows"

  override val excluded = List(PureTripleChows, TileHog, AllChows)

  def find(m: HuLe): Result =
    SomeResult(m.allChows) {
      m.allChows.count(_ == m.allChows(0)) == 4
    }

}

object PureTerminalChows extends Combination {
  val id = 13
  val points = 64
  val name = "Pure terminal chows"
  val description = "2 times 123, 789 and a pair of 5 in one family"

  override val excluded = List(FullFlush, AllChows, TwoTerminalChows, PureDoubleChows)

  def find(m: HuLe): Result =
    SomeResult(m.allFigures) {
      m.numberOfStraightFamily == 1 &&
        m.allChows.size == 4 &&
        m.allDuis.size == 1 && {
        val fam = m.allChows.head.family
        m.allChows.count(c => c.family == fam && c.isStartingChow) == 2 &&
          m.allChows.count(c => c.family == fam && c.isEndingChow) == 2 &&
          m.allDuis(0).tile == Tile(fam, 5)
      }
    }

}

object FourConcealedPungs extends Combination {
  val id = 12
  val points = 64
  val name = "Four Concealed Pungs"
  val description = "Four concealed pungs"

  override val excluded = List(ThreeConcealedPungs)

  def find(m: HuLe): Result = {
    SomeResult(m.allPungsLike) {
      m.allConcealedPungLike.size == 4
    }
  }

}

object AllHonors extends Combination {
  val id = 11
  val points = 64
  val name = "All Honors"
  val description = "Only winds and dragons"

  override val excluded = List(AllTerminalsAndHonors, AllPungs, PungOfTerminalsOrHonors)

  def find(m: HuLe): Result =
    SomeResult(m.allFigures) {
      m.allTiles.forall(_.isHonor)
    }

}

object LittleThreeDragons extends Combination {
  val id = 10
  val points = 64
  val name = "Little three dragons"
  val description = "2 pungs of dragon and a pair of dragon"

  override val excluded = List(TwoDragonPungs)

  def find(m: HuLe): Result =
    SomeResult(m.allDragonFigures) {
      m.allDragonFigures.size == 3 &&
        m.allDragonPungsLike.size == 2
    }

}

object LittleFourWinds extends Combination {
  val id = 9
  val points = 64
  val name = "Little four winds"
  val description = "3 pungs of wind and a pair of wind"

  //BigThreeWind is implied because it can exist Pung of Terminal or honor
  override val implied = List(BigThreeWind)

  def find(m: HuLe): Result =
    SomeResult(m.allWindFigures) {
      m.allWindFigures.size == 4 && m.allWindPungsLike.size == 3
    }

}

object AllTerminals extends Combination {
  val id = 8
  val points = 64
  val name = "All Terminals"
  val description = "only 1 and 9"

  //BigThreeWind is implied because it can exist Pung of Terminal or honor
  override val excluded = List(AllPungs, AllTerminalsAndHonors, PungOfTerminalsOrHonors, NoHonors)

  def find(m: HuLe): Result =
    SomeResult(m.allFigures) {
      m.allTiles.forall(_.isTerminal)
    }

}


object ThirteenOrphansComb extends Combination {
  val id = 7
  val points = 88
  val name = "Thirteen Orphans"
  val description = "1 and 9 in three families and 7 distinct honors plus 1 extra honor"

  override val excluded = List(AllTerminalsAndHonors, ConcealedHand)

  def find(m: HuLe): Result =
    Result(m.allThirteenOrphans)

}

object SevenShiftedPairs extends Combination {
  val id = 6
  val points = 88
  val name = "Seven shifted pairs"
  val description = "7 consecutive pairs in the same family"

  override val excluded = List(SevenPairs, FullFlush)

  def find(m: HuLe): Result =
    SomeResult(m.allFigures) {
      m.allDuis.size == 7 &&
        m.numberOfStraightFamily == 1 && {
        val fam = m.allDuis(0).family
        val firstValue = m.allDuis(0).value
        val lastValue = firstValue + 7
        (firstValue until lastValue).forall(i => m.allDuis.contains(Dui(Tile(fam, i))))
      }
    }

}

object FourKongs extends Combination {
  val id = 5
  val points = 88
  val name = "Four kongs"
  val description = "Four kongs"

  override val excluded = List(AllPungs, SingleWait)

  def find(m: HuLe): Result =
    SomeResult(m.allKongs) {
      m.allKongs.size == 4
    }

}

object NineGates extends Combination {
  val id = 4
  val points = 88
  val name = "Nine gates"
  val description = "1-1-1-2-3-4-5-6-7-8-9-9-9 + another of the same family"

  override val excluded = List(FullFlush, ConcealedHand, PungOfTerminalsOrHonors)

  def find(m: HuLe): Result =
    SomeResult(m.allFigures) {
      m.numberOfStraightFamily == 1 && {
        m.allTiles.head.isStraight && {
          val fam = m.allTiles.head.family

          def t(value: Int, occ: Int) = (Tile(fam, value), occ)
          val pattern = List[Types.TileOccurence](t(1, 3), t(2, 1), t(3, 1), t(4, 1), t(5, 1), t(6, 1), t(7, 1), t(8, 1), t(9, 3))
          val actual = TileSet(m.allTiles).tocs

          pattern.zip(actual).forall {
            case ((t1, occMin), (t2, occActual)) => t1 == t2 && occActual >= occMin
          }
        }
      }

    }

}

object AllGreen extends Combination {
  val id = 3
  val points = 88
  val name = "All Green"
  val description = "Only green tiles: 2,3,4,6,8 bamboos and green dragon"

  def find(m: HuLe): Result =
    SomeResult(m.allFigures) {
      val allowed = List(b2, b3, b4, b6, b8, dg)
      m.allTiles.forall(allowed.contains(_))
    }

}

object BigThreeDragons extends Combination {
  val id = 2
  val points = 88
  val name = "Big three dragons"
  val description = "Three pungs of dragon"

  override val excluded = List(TwoDragonPungs)

  def find(m: HuLe): Result =
    SomeResult(m.allDragonPungsLike) {
      m.allDragonPungsLike.size == 3
    }

}

object BigFourWinds extends Combination {
  val id = 1
  val points = 88
  val name = "Big Four Winds"
  val description = "4 wind pungs"

  override val excluded = List(BigThreeWind, AllPungs, PrevalentWind,
    SeatWind, PungOfTerminalsOrHonors)

  def find(m: HuLe): Result =
    SomeResult(m.allWindPungsLike) {
      m.allWindPungsLike.size == 4
    }

}


