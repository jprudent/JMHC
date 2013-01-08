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
  def apply(figures: Figures)(cond: Boolean): Result =
    apply(List(figures)) {
      cond
    }


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

  def findTwoTerminalChows(chows: List[Chow]) =
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

object SelfDrawnComb extends Combination {
  val id = 80
  val points = 1
  val name = "Self Drawn"
  val description = "finish with self drawing"


  override def find(m: HuLe): Result = {
    m.lastTileContext.origin == SelfDrawn match {
      case true => {

        m.closed.find(_.asList.contains(m.lastTileContext.tile)) match {
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


object PungOfTerminalOrHonors extends Combination {
  val id = 73
  val points = 1
  val name = "Pung of Terminals or Honor"
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
    Result(Combination.findTwoTerminalChows(m.allChows))
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

  def find(m: HuLe): Result = {
    val allDoublePung =
      for {
        p1 <- m.allStraightPungLike
        p2 <- m.allStraightPungLike if Tile.ord.compare(p1.tile, p2.tile) < 0 && p2.tile.value == p1.tile.value
      } yield (List(p1, p2).sorted(OrdFigure))

    Result(allDoublePung)
  }
}

object TileHog extends Combination {
  val id = 64
  val points = 2
  val name = "Tile Hog"
  val description = "4 identical tiles in at least 2 figures"

  def find(m: HuLe): Result = {
    val allFiguresButKong = m.allFigures.filterNot(_.isInstanceOf[Kong])
    val allTilesButKong = allFiguresButKong.map(f => f.asList).flatten
    val allTileHogs = TileSet(allTilesButKong).tocs.filter {
      case (tile, occurence) if occurence == 4 => true
      case _ => false
    }.map {
      case (tile, occurence) => allFiguresButKong.filter(_.asList.contains(tile))
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
    if (m.allDuis.size == 1 && m.allChows.size == 4)
      Result(m.allChows)
    else
      EmptyResult
}


object ConcealedHand extends Combination {
  val id = 62
  val points = 2
  val name = "Concealed Hand"
  val description = "Nothing melded but finish on a discard"

  def find(m: HuLe): Result = {

    m.melded == Nil && m.lastTileContext.origin == Discarded match {
      case true => Result(m.allFigures)
      case false => EmptyResult
    }
  }
}


object SeatWind extends Combination {
  val id = 61
  val points = 2
  val name = "Seat Wind"
  val description = "1 pung of player's wind"


  override val implied = List(PungOfTerminalOrHonors)

  def find(m: HuLe): Result =
    m.allPungsLike.find(_.tile.family == m.context.seatWind) match {
      case None => EmptyResult
      case Some(pung) => Result(pung)
    }
}

object PrevalentWind extends Combination {
  val id = 60
  val points = 2
  val name = "Prevalent Wind"
  val description = "1 pung of round wind"


  override val implied = List(PungOfTerminalOrHonors)

  def find(m: HuLe): Result =
    m.allPungsLike.find(_.tile.family == m.context.prevalentWind) match {
      case None => EmptyResult
      case Some(pung) => Result(pung)
    }
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
    if (m.lastTileContext.lastTileSituation == LastTileOfKind) Result(SingleTile(m.lastTileContext.tile))
    else EmptyResult
}

object TwoMeldedKongs extends Combination {
  val id = 57
  val points = 4
  val name = "Two melded kongs"
  val description = "Two melded kongs or one melded and one concealed kongs"

  override val excluded = List(MeldedKong)

  def find(m: HuLe): Result = {
    if (m.allMeldedKongs.size == 2 || (m.allMeldedKongs.size == 1 && m.concealedKongs.size == 1)) {
      Result((m.allMeldedKongs ::: m.concealedKongs).sorted(OrdFigure))
    }
    else {
      EmptyResult
    }
  }

}


object FullyConcealedHand extends Combination {
  val id = 56
  val points = 4
  val name = "Fully Concealed Hand"
  val description = "Nothing melded, finish on self drawn"


  override val implied = List(SelfDrawnComb)

  def find(m: HuLe): Result =
    m.lastTileContext.origin == SelfDrawn && m.melded.size == 0
    match {
      case true => Result(m.allFigures)
      case false => EmptyResult
    }
}

object OutsideHand extends Combination {
  val id = 55
  val points = 4
  val name = "Outside Hand"
  val description = "At least 1 honor or 1 terminal in each figure"

  def find(m: HuLe): Result = {

    m.allFigures.forall(figure => figure match {
      case f: SomeKnittedWithSomeDragons => false
      case f: Knitted => false
      case f: ThirteenOrphans => false
      case f => f.asList.exists(_.isTerminalOrHonor)
    })

    match {
      case true => Result(m.allFigures)
      case false => EmptyResult
    }
  }
}

object TwoDragonPungs extends Combination {
  val id = 54
  val points = 6
  val name = "Two dragon pungs"
  val description = "Two dragon pungs"

  override val excluded = List(DragonPung)

  def find(m: HuLe): Result = {
    m.allDragonPungsLike.size == 2 match {
      case true => Result(m.allDragonPungsLike)
      case false => EmptyResult
    }
  }
}


object MeldedHand extends Combination {
  val id = 53
  val points = 6
  val name = "Melded Hand"
  val description = "4 figures melded, and finish on discard"

  override val excluded = List(SingleWait)

  def find(m: HuLe): Result = {
    val cond = m.allFigures.size == 5 && m.melded.size == 4 && m.lastTileContext.origin != SelfDrawn
    if (cond)
      Result(m.allFigures)
    else
      EmptyResult
  }
}

object AllTypes extends Combination {
  val id = 52
  val points = 6
  val name = "All Types"
  val description = "5 figures in all families (bamboo, character, stone, wind, honor)"


  def find(m: HuLe): Result = {

    def hasFamily(family: Family, figures: Figures) =
      figures.exists(figure => figure match {
        case f: Knitted if family.isInstanceOf[StraightFamily] => true
        case f: SomeKnittedWithSomeDragons => true
        case f => f.asList.forall(_.family == family)
      })


    List(List(Bamboo), List(Character), List(Stone),
      List(EastWind, NorthWind, WestWind, SouthWind),
      List(RedDragon, WhiteDragon, GreenDragon))
      .forall(listFamily => listFamily.exists(family => hasFamily(family, m.allFigures)))
    match {
      case true => Result(m.allFigures)
      case false => EmptyResult
    }

  }
}

object MixedShiftedChow extends Combination {
  val id = 51
  val points = 6
  val name = "Mixed Shifted Chow"
  val description = "3 chows in 3 families shifted by 2 tiles"

  def find(m: HuLe): Result = {
    val allShiftedChows = for {chow1 <- m.allChows
                               chow2 <- m.allChows if chow2.t1.value == chow1.t1.value + 1 && chow2.t1.family != chow1.t1.family
                               chow3 <- m.allChows if chow3.t1.value == chow2.t1.value + 1 && chow3.t1.family != chow1.t2.family && chow3.t1.family != chow1.t1.family
    } yield (List(chow1, chow2, chow3).sorted(OrdChow))

    //Every mixed shifted chow is scored
    Result(allShiftedChows)
  }
}


object HalfFlush extends Combination {
  val id = 50
  val points = 6
  val name = "Half Flush"
  val description = "all tiles of the same type (bamboos, character or stone) plus some honors "


  def find(m: HuLe): Result = {
    val (honors, straight) = m.allTiles.partition(_.family.isInstanceOf[HonorFamily])

    honors.size > 0 &&
      straight.map(_.family).toSet.size == 1
    match {
      case true => Result(m.allFigures)
      case false => EmptyResult
    }
  }
}

object AllPungs extends Combination {
  val id = 49
  val points = 6
  val name = "All Pungs"
  val description = "All Pungs and one pair"


  def find(m: HuLe): Result = {
    if (m.allPungsLike.size == 4) Result(m.allPungsLike)
    else EmptyResult
  }

}

object TwoConcealedKongs extends Combination {
  val id = 48
  val points = 8
  val name = "Two concealed kongs"
  val description = "Two concealed kong"

  override val excluded = List(TwoConcealedPungs)

  def find(m: HuLe): Result = {
    m.concealedKongs.size == 2 match {
      case true => Result(m.concealedKongs)
      case false => EmptyResult
    }
  }

}

object RobbingTheKong extends Combination {
  val id = 47
  val points = 8
  val name = "Robbing the kong"
  val description = "Finish by robbing the tile of another player which transform a pung to a kong"

  def find(m: HuLe): Result = {
    m.lastTileContext.origin == KongRobbed match {
      case true => Result(SingleTile(m.lastTileContext.tile))
      case false => EmptyResult
    }
  }

}

object OutWithRemplacementTile extends Combination {
  val id = 46
  val points = 8
  val name = "Out with replacement tile"
  val description = "Finish with the tile that replace the kong tile"

  def find(m: HuLe): Result = {
    m.lastTileContext.origin == ReplacedTile match {
      case true => Result(SingleTile(m.lastTileContext.tile))
      case false => EmptyResult
    }
  }
}

object LastTileClaimComb extends Combination {
  val id = 45
  val points = 8
  val name = "Last tile claim"
  val description = "Finish with a claim of the last discarded tile of the game"

  def find(m: HuLe): Result = {
    m.lastTileContext.lastTileSituation == LastTileClaim match {
      case true => Result(SingleTile(m.lastTileContext.tile))
      case false => EmptyResult
    }
  }
}

object LastTileDrawComb extends Combination {
  val id = 44
  val points = 8
  val name = "Last tile draw"
  val description = "Finish with a claim of the last discarded tile of the game"

  override val excluded = List(SelfDrawnComb)

  def find(m: HuLe): Result = {
    m.lastTileContext.lastTileSituation == LastTileDraw match {
      case true => Result(SingleTile(m.lastTileContext.tile))
      case false => EmptyResult
    }
  }
}

object ChickenHand extends Combination {
  val id = 43
  val points = 8
  val name = "Chicken Hand"
  val description = "0 point hand, except flowers"

  override val excluded = List(SelfDrawnComb)

  def find(m: HuLe): Result = {
    val allForbidenThings: List[Combination] = List(
      ThirteenOrphansComb,
      SevenPairs,
      LesserHonorsAndKnittedTiles,
      KnittedStraight,
      MixedStraight,
      //TODO reversible tiles
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
      PungOfTerminalOrHonors,
      MeldedKong,
      OneVoidedSuit,
      NoHonors,
      EdgeWait,
      ClosedWait,
      SingleWait,
      SelfDrawnComb)

    val noPoints = allForbidenThings.forall(_.find(m) == EmptyResult)

    if (noPoints) Result(m.allFigures)
    else EmptyResult
  }
}


object MixedShiftedPung extends Combination {
  val id = 42
  val points = 8
  val name = "Mixed Shifted Pung"
  val description = "Three consecutive pungs in three families"

  def find(m: HuLe): Result = {

    val straightPungs = m.allPungsLike.filter(_.tile.isStraight)

    def cond(p1: PungLike, p2: PungLike) =
      p1.tile.family != p2.tile.family && p2.tile.value == p1.tile.value + 1

    val allMixedShiftedPungs: List[Figures] =
      for {p1 <- straightPungs
           p2 <- straightPungs if cond(p1, p2)
           p3 <- straightPungs if cond(p2, p3)
      } yield (List(p1, p2, p3).sorted(OrdFigure))

    Result(allMixedShiftedPungs)

  }
}

object MixedTripleChow extends Combination {
  val id = 41
  val points = 8
  val name = "Mixed Triple Chow"
  val description = "Three identical chows in three families"


  override val implied = List(MixedDoubleChows)

  def find(m: HuLe): Result = {
    //FIXME j'ai comme un doute que ?a match 3 chows dans 2 familles
    val resolved: List[Figure] = m.allChows
      .groupBy(_.t1.value) // Map[Int, List[Chow]
      .values.toList // List[List[Chow]]
      .filter(_.size == 3) //FIXME ???
      .flatten // List[Chow]

    //TODO dans le cas o? 4 chows identique, combien de fois faut-il compter cette combinaison ???
    Result(resolved)

  }
}

object ReversibleTiles extends Combination {
  val id = 40
  val points = 8
  val name = "Reversible Tiles"
  val description = "Only 1,2,3,4,5,8,9 stone, 2,4,5,6,8,9 Bamboos, White Dragon"


  override val excluded = List(OneVoidedSuit)

  def find(m: HuLe): Result = {
    val allowedTiles = List(
      s1, s2, s3, s4, s5, s6, s7, s8, s9,
      b2, b4, b5, b6, b8, b9,
      dw
    )

    m.allTiles.forall(allowedTiles.contains(_)) match {
      case true => Result(m.allFigures)
      case false => EmptyResult
    }

  }
}


object MixedStraight extends Combination {
  val id = 39
  val points = 8
  val name = "Mixed Straight"
  val description = "Three consecutive chows in three family"

  def find(m: HuLe): Result = {
    val allMixedStraight =
      for {
        chow1 <- m.allChows if chow1.t1.value == 1
        chow2 <- m.allChows if !chow1.sameFamily(chow2) && chow2.t1.value == 4
        chow3 <- m.allChows if !chow2.sameFamily(chow3) && !chow1.sameFamily(chow3) && chow3.t1.value == 7
      } yield (List(chow1, chow2, chow3).sorted(OrdFigure))

    Result(allMixedStraight)
  }
}

object BigThreeWind extends Combination {
  val id = 38
  val points = 12
  val name = "Big Three Winds"
  val description = "3 pungs of each wind"

  override val implied = List(PungOfTerminalOrHonors)

  def find(m: HuLe): Result = {

    val winds = List(we, wn, ww, ws)

    val allWindPungs = m.allPungsLike.filter(p => winds.contains(p.tile))

    if (allWindPungs.size == 3) Result(allWindPungs)
    else EmptyResult
  }
}

object LowerFour extends Combination {
  val id = 37
  val points = 12
  val name = "Lower Four"
  val description = "Only 1, 2, 3, 4"

  override val excluded = List(NoHonors)

  def find(m: HuLe): Result = {
    m.allTiles.forall(t => t.family.isInstanceOf[StraightFamily] && t.value <= 4) match {
      case false => EmptyResult
      case true => Result(m.allFigures)
    }
  }
}


object UpperFour extends Combination {
  val id = 36
  val points = 12
  val name = "Upper Four"
  val description = "Only 9, 8, 7, 6"

  override val excluded = List(NoHonors)

  def find(m: HuLe): Result = {
    m.allTiles.forall(t => t.family.isInstanceOf[StraightFamily] && t.value >= 6) match {
      case false => EmptyResult
      case true => Result(m.allFigures)
    }
  }
}

object KnittedStraight extends Combination {
  val id = 35
  val points = 12
  val name = "Knitted Straight"
  val description = "1-4-7 in one family, 2-5-8 in the second family, 3-6-9 in the third family"


  def find(m: HuLe): Result = {
    val allKnittedTiles = m.closed.filter(_.isInstanceOf[Knitted])
    Result(allKnittedTiles)
  }
}

object LesserHonorsAndKnittedTiles extends Combination {
  val id = 34
  val points = 12
  val name = "Lesser Honors and Knitted Tiles"
  val description = "6 unique honors and incomplete knitted straight OR 5 unique honors and complete knitted straight"

  override val excluded = List(AllTypes, ConcealedHand)

  def find(m: HuLe): Result = {
    val allKnittedWithDragons = m.closed.filter(_.isInstanceOf[SomeKnittedWithSomeDragons])
    Result(allKnittedWithDragons)
  }
}

object ThreeConcealedPungs extends Combination {
  val id = 33
  val points = 16
  val name = "Three concealed pungs"
  val description = "Three concealed pungs"

  override val excluded = List(TwoConcealedPungs)

  def find(m: HuLe): Result = {
    m.allConcealedPungLike.size == 3 match {
      case true => Result(m.allConcealedPungLike)
      case false => EmptyResult
    }
  }
}

object TriplePungs extends Combination {
  val id = 32
  val points = 16
  val name = "Triple Pungs"
  val description = "Three identical pungs in three family"

  override val excluded = List(DoublePung)

  def find(m: HuLe): Result = {
    val allTriplePung =
      for {
        p1 <- m.allStraightPungLike
        p2 <- m.allStraightPungLike if Tile.ord.compare(p1.tile, p2.tile) < 0 && p2.tile.value == p1.tile.value
        p3 <- m.allStraightPungLike if Tile.ord.compare(p2.tile, p3.tile) < 0 && p3.tile.value == p2.tile.value
      } yield (List(p1, p2, p3).sorted(OrdFigure))

    Result(allTriplePung)
  }
}

object AllFive extends Combination {
  val id = 31
  val points = 16
  val name = "All Five"
  val description = "5s in each figures"

  override val excluded = List(AllSimples)

  def find(m: HuLe): Result = {
    m.standardHand && m.allFigures.forall(_.asList.exists(_.value == 5)) match {
      case true => Result(m.allFigures)
      case false => EmptyResult
    }
  }
}


object PureShiftedChow extends Combination {
  val id = 30
  val points = 16
  val name = "Pure shifted chows"
  val description = "3 chows shifted by one or two tiles in the same family"

  def find(m: HuLe): Result = {
    val chowsByFamily = m.allChows.groupBy(_.t1.family)
    def matchingPureShiftedChow(chows: List[Chow]): List[List[Chow]] = {
      for {
        c1 <- chows
        c2 <- chows if c2.t1.value == c1.t1.value + 1 || c2.t1.value == c1.t1.value + 2
        c3 <- chows if c3.t1.value == c2.t1.value + (c2.t1.value - c1.t1.value)
      } yield (List(c1, c2, c3))
    }

    val allPureShiftedChows = chowsByFamily.map {
      case (family, chows) => matchingPureShiftedChow(chows)
    }.filterNot(_ == Nil).flatten.toList

    Result(allPureShiftedChows)
  }
}

object ThreeSuitedTerminalChows extends Combination {
  val id = 29
  val points = 16
  val name = "Three Suited Terminal Chows"
  val description = "1,2,3,7,8,9 in two family and a pair of 5 in the third"

  override val excluded = List(AllChows, MixedDoubleChows, PureDoubleChows, TwoTerminalChows)

  def find(m: HuLe): Result = {
    //quick fail
    if (m.allPungsLike.size != 0 || !m.standardHand) return EmptyResult

    val groupedChows = m.allChows.groupBy(_.t1.family)

    val terminalChows = groupedChows.map {
      case (family, chows) => Combination.findTwoTerminalChows(chows)
    }.filterNot(_ == Nil)

    def hasPairOf5InThirdFamily() = {
      m.allDuis.exists(d => d.tile.value == 5 && !m.allChows.map(_.t1.family).toSet.exists(_ == d.tile.family))
    }

    if (terminalChows.size == 2 && hasPairOf5InThirdFamily())
      Result(m.allFigures)
    else
      EmptyResult

  }
}


object FullFlush extends Combination {
  val id = 22
  val points = 24
  val name = "Full Flush"
  val description = "Only tiles of the same straight family"


  def find(m: HuLe): Result = {
    m.allTiles.groupBy(tile => tile.family).size == 1 match {
      case true => Result(m.allFigures)
      case false => EmptyResult
    }
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
    if (m.closed.size == 7 && m.closed.forall(_.isInstanceOf[Dui]))
      Result(m.closed)
    else EmptyResult
}

object AllTerminalsAndHonors extends Combination {
  val id = 18
  val points = 32
  val name = "All Terminals And Honors"
  val description = "only 1, 9 and honors"


  override val excluded = List(OutsideHand)

  def find(m: HuLe): Result = {
    m.allTiles.forall {
      case Tile(family, value) if value == 1 || value == 9 || family.isInstanceOf[HonorFamily] => true
      case _ => false
    }
    match {
      case true => Result(m.allFigures)
      case false => EmptyResult
    }
  }
}

object ThreeKongs extends Combination {
  val id = 17
  val points = 32
  val name = "Three Kongs"
  val description = "Three Kongs"

  def find(m: HuLe): Result = {
    m.allKongs.size == 3 match {
      case true => Result(m.allKongs)
      case false => EmptyResult
    }
  }

}

object AllHonors extends Combination {
  val id = 11
  val points = 64
  val name = "All Honors"
  val description = "Only winds and dragons"

  override val excluded = List(AllTerminalsAndHonors, AllPungs, PungOfTerminalOrHonors)

  def find(m: HuLe): Result = {
    m.allTiles.forall(!_.isStraight) match {
      case true => Result(m.allFigures)
      case false => EmptyResult
    }
  }

}


object ThirteenOrphansComb extends Combination {
  val id = 7
  val points = 88
  val name = "Thirteen Orphans"
  val description = "1 and 9 in three families and 7 distinct honors plus 1 extra honor"

  override val excluded = List(AllTerminalsAndHonors, ConcealedHand)

  def find(m: HuLe): Result = {
    val allThirteenOrphans = m.closed.filter {
      _.isInstanceOf[ThirteenOrphans]
    }
    Result(allThirteenOrphans)
  }
}

object BigThreeDragons extends Combination {
  val id = 2
  val points = 88
  val name = "Big three dragons"
  val description = "Three pungs of dragon"

  override val excluded = List(DragonPung)

  def find(m: HuLe): Result = {
    m.allDragonPungsLike.size == 3 match {
      case true => Result(m.allDragonPungsLike)
      case false => EmptyResult
    }

  }
}

