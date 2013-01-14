package org.liprudent.majiang.tiles

import Types.TileOccurence
import Types.Figures

import org.liprudent.majiang.figures._


/**
 * This class will find all possibles set of figures that can be made with a set of tiles
 *
 * @param tileSet
 */
case class FiguresComputer(tileSet: TileSet) {


  /**
   * an ordered list of possible pungs
   */
  lazy val pungs: List[Pung] = {
    tileSet.tocs.filter(t => t._2 >= PungProperties.size).map {
      t => new Pung(t._1)
    }
  }

  /**
   * an ordered list of possible chows
   */
  lazy val chows: List[Chow] = {
    sublistsOf(Chow.size, allSuits).map(new Chow(_))
  }

  /**
   * an ordered list of possible duis
   */
  lazy val duis: List[Dui] = {
    tileSet.tocs.filter(t => t._2 >= DuiProperties.size).map {
      t => {
        val d = new Dui(t._1)
        if (t._2 == 4) List(d, d)
        else List(d)
      }
    }.flatten
  }

  lazy val knitted: List[Knitted] = {

    //quick fail
    if (tileSet.size < 9 || chows.size >= 2 || pungs.size >= 2) Nil

    else {
      List(Bamboo, Character, Stone)
        .permutations // all combinations of those 3 families
        .map(families => Knitted(families(0), families(1), families(2))).toList // as Knitted
        .filter(knitted => knitted.toTiles.forall(tile => tileSet.exists(t => t == tile))) // where each knitted tile
        // exists in tileSet
        .toList
    }
  }

  lazy val someKnittedSomeDragons: List[SomeKnittedWithSomeDragons] = {

    //quick fail
    if (tileSet.size != 14) Nil

    else {
      val honors = tileSet.filter(!_.family.isInstanceOf[StraightFamily])
      if (hasTwins(honors) || honors.size < 5) {
        Nil
      }
      else {
        val knitted = tileSet.filter(_.family.isInstanceOf[StraightFamily])
        val knittedComputer: FiguresComputer = FiguresComputer(tileSet)
        if (knitted.size != 14 - honors.size || hasChowsOrTwins(knittedComputer)) {
          Nil
        }
        else {

          val tiles147 = knitted.filter(tile => tile.value == 1 || tile.value == 4 || tile.value == 7)
          val tiles258 = knitted.filter(tile => tile.value == 2 || tile.value == 5 || tile.value == 8)
          val tiles369 = knitted.filter(tile => tile.value == 3 || tile.value == 6 || tile.value == 9)
          val knitteds: List[TileSet] = List(tiles147, tiles258, tiles369)

          //all tiles should be the same family
          if (knitteds.forall(tileSet => tileSet.sameFamily)) {
            val allKnitted: List[Tile] = knitteds.map(_.toTiles).flatten.sorted
            assert(allKnitted.size == 14 - honors.size)
            List(SomeKnittedWithSomeDragons(allKnitted, honors.toTiles))
          } else {
            Nil
          }
        }
      }
    }
  }

  lazy val thirteenOrphans: List[ThirteenOrphans] = {
    val containsFixed = ThirteenOrphansProperties.fixedTiles.forall(tile => tileSet.exists(_ == tile))
    if (containsFixed) {
      val extra = tileSet.tocs.filter(toc => toc._2 == 2)
      extra match {
        case (tile, occ) :: Nil if tile.family.isInstanceOf[HonorFamily] => List(ThirteenOrphans(tile))
        case _ => Nil
      }
    }
    else {
      Nil
    }
  }

  private def hasTwins(tileSet: TileSet) =
    tileSet.tocs.forall(_._2 >= 2)

  private def hasChowsOrTwins(computer: FiguresComputer) =
    List(computer.chows, computer.pungs, computer.duis).exists(_.size != 0)

  /**
   * all possible figures
   * <p>
   * result is ordered : pungs, chows, duis
   */
  lazy val allFigures: List[Figure] = (thirteenOrphans ::: someKnittedSomeDragons ::: knitted ::: duis ::: chows ::: pungs).sorted(OrdFigure)

  /**
   *
   * @return All possible combinations of figures. Each element of the set is an ordered list of figures.
   */
  lazy val allFiguresCombinations: Set[Figures] = findFigures(this)

  //TODO optimization: it's useless to try all combinations for a given Figure type
  //List(Chow(b1,b2,b3), Chow(b2,b3,b4)) == List(Chow(b2,b3,b4), Chow(b1,b2,b3))
  protected def findFigures(figuresComputer: FiguresComputer): Set[Figures] = {
    figuresComputer.allFigures match {
      case Nil => Set()
      case all => {
        all.map {
          figure => {
            val next: Set[Figures] = findFigures(FiguresComputer(figuresComputer.tileSet.removed(figure.toTiles)))
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


  /**
   * an ordered list of possible length free suits
   */
  protected lazy val allSuits: List[Suit] = {
    splitByFamily.map {
      tilesSameFamily =>
        findSuits(TileSet(tilesSameFamily))
    }.flatten
  }

  /**
   * a list of sub-list containing ordered TileOccurence of the same family
   */
  protected[tiles] val splitByFamily: List[List[TileOccurence]] = {
    tileSet.tocs.groupBy {
      e => e._1.family
    }.values.toList.sorted(OrdListTileOccurence)
  }


  /**
   * Search for the longest suits.
   * @param tiles where to find suits.
   * @return the list of the longest suits
   */
  protected[tiles] def findSuits(tiles: TileSet): List[Suit] = {

    def findSuit(tiles: List[TileOccurence], prev: Tile): Suit = {
      tiles match {
        case h :: t if prev.previousOf(h._1) => prev :: findSuit(t, h._1)
        case _ => List(prev)
      }
    }

    tiles match {
      case TileSet(Nil) => Nil
      case TileSet(h :: t) => {
        val suit = findSuit(t, h._1)
        suit :: findSuits(tiles.removed(suit))
      }
    }

  }


  /**
   * Find all sublists of fixed length in given suits
   * @param suitSize sublists fixed length
   * @param suits the suits where to find sublists. Theses suits can have any length.
   * @return A list of sub-lists with fixed length
   */
  protected[tiles] def sublistsOf(suitSize: Int, suits: List[Suit]): List[Suit] = {
    require(suitSize > 0)

    def listsOf(suitSize: Int, suit: Suit): List[Suit] = {
      findSubSuitsIndices(suit.size, suitSize).map {
        indices: List[Int] =>
          indices.map(i => suit(i))
      }
    }

    suits.map(listsOf(suitSize, _)).flatten
  }


  /**
   * Computes all fixed <code>length</code> sub-suit indices of a suit of tiles which length is
   * <code>suitSize</code>.
   * @param suitSize The list where to compute sub-lists indices from
   * @param length The fixed length of each sub-suit
   * @return a list of list of indices. Indices are sorted ascending and lists too.
   *
   **/
  protected[tiles] def findSubSuitsIndices(suitSize: Int, length: Int): List[List[Int]] = {
    val nbElements = suitSize - length + 1
    (for {i <- 0 until nbElements} yield (for {j <- i until i + length} yield j).toList).toList
  }
}

object FiguresComputer {
  def apply(tiles: List[Tile]): FiguresComputer = FiguresComputer(TileSet(tiles))
}
