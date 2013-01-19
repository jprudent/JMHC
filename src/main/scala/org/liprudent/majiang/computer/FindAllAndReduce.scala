package org.liprudent.majiang.computer

import org.liprudent.majiang.tiles._
import org.liprudent.majiang.figures._
import org.liprudent.majiang.tiles.Types._
import org.liprudent.majiang.figures.ThirteenOrphans
import org.liprudent.majiang.figures.Knitted
import org.liprudent.majiang.figures.Pung
import org.liprudent.majiang.figures.Dui
import org.liprudent.majiang.figures.SomeKnittedWithSomeDragons

/**
 * This class will find all possibles set of figures
 * that can be made with a set of tiles
 *
 * @param tileSet
 */
case class FindAllAndReduce(tileSet: TileSet) extends TilesToFiguresService {

  /**
   * @return All possible combinations of figures. Each element of the set is a valid ordered list of figures.
   */
  lazy val allFiguresCombinations: Set[Figures] = findFigures(this)

  /**
   * an ordered list of possible pungs
   */
  protected[computer] val pungs: List[Pung] = {
    tileSet.tocs.filter(t => t._2 >= Pung.size).map {
      t => new Pung(t._1)
    }
  }

  /**
   * an ordered list of possible chows
   */
  protected[computer] lazy val chows: List[Chow] = {
    FindAllAndReduce.sublistsOf(Chow.size, allSuits).map(new Chow(_))
  }

  /**
   * an ordered list of possible duis
   */
  protected[computer] lazy val duis: List[Dui] = {
    tileSet.tocs.filter(t => t._2 >= DuiProperties.size).map {
      t => {
        val d = new Dui(t._1)
        if (t._2 == 4) List(d, d)
        else List(d)
      }
    }.flatten
  }

  /**
   * an ordered list of possible knitted
   */
  protected[computer] lazy val knitted: List[Knitted] = {

    //quick fail
    if (tileSet.size < 9 || chows.size >= 2 || pungs.size >= 2) Nil
    else {
      List(Bamboo, Character, Stone).permutations // all combinations of those 3 families
        .map(families => Knitted(families(0), families(1), families(2))).toList // as Knitted
        .filter(knitted => knitted.toTiles.forall(tile => tileSet.exists(t => t == tile))) // where each knitted tile
        // exists in tileSet
        .toList
    }
  }

  /**
   * An ordered list of possible knitted + dragons
   */
  protected[computer] lazy val someKnittedSomeDragons: List[SomeKnittedWithSomeDragons] = {

    //quick fail
    if (tileSet.size != 14) Nil
    else {
      val honors = tileSet.filter(_.isHonor)
      if (hasTwins(honors) || honors.size < 5) {
        Nil
      } else {
        val knitted = tileSet.filter(_.isStraight)
        val knittedComputer: FindAllAndReduce = FindAllAndReduce(tileSet)
        if (knitted.size != 14 - honors.size || hasChowsOrTwins(knittedComputer)) {
          Nil
        } else {

          val tiles147 = knitted.filter(tile => tile.value == 1 || tile.value == 4 || tile.value == 7)
          val tiles258 = knitted.filter(tile => tile.value == 2 || tile.value == 5 || tile.value == 8)
          val tiles369 = knitted.filter(tile => tile.value == 3 || tile.value == 6 || tile.value == 9)
          val knitteds: List[TileSet] = List(tiles147, tiles258, tiles369)

          //all tiles should be the same family
          if (knitteds.forall(tileSet => tileSet.isSingleFamily)) {
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

  /**
   * An ordered list of possible thirteen orphans
   */
  protected[computer] lazy val thirteenOrphans: List[ThirteenOrphans] = {
    val containsFixed = ThirteenOrphans.fixedTiles.forall(tile => tileSet.exists(_ == tile))
    if (containsFixed) {
      val extra = tileSet.tocs.filter(toc => toc._2 == 2)
      extra match {
        case (tile, occ) :: Nil if tile.family.isInstanceOf[HonorFamily] => List(ThirteenOrphans(tile))
        case _ => Nil
      }
    } else {
      Nil
    }
  }

  /**
   * all possible figures
   * <p>
   * result is ordered
   */
  protected[computer] lazy val allFigures: List[Figure] =
    thirteenOrphans ::: someKnittedSomeDragons ::: knitted ::: pungs ::: chows ::: duis

  //TODO optimization: it's useless to try all combinations for a given Figure type
  //List(Chow(b1,b2,b3), Chow(b2,b3,b4)) == List(Chow(b2,b3,b4), Chow(b1,b2,b3))
  protected def findFigures(figuresComputer: FindAllAndReduce): Set[Figures] = {
    figuresComputer.allFigures match {
      case Nil => Set()
      case all => {
        all.map {
          figure => {
            val next: Set[Figures] = findFigures(FindAllAndReduce(figuresComputer.tileSet.removed(figure.toTiles)))
            next match {
              case s if s.isEmpty => Set(List(figure))
              case _ => next.map(figures => (figure :: figures))
            }
          }
        }.flatten.toSet[Figures].map(figures => figures.sorted(OrdFigure))
      }
    }
  }

  /**
   * an ordered list of possible length free suits
   */
  protected lazy val allSuits: List[Suit] = {
    tileSet.splitByFamily.map {
      tilesSameFamily =>
        FindAllAndReduce.findSuits(TileSet(tilesSameFamily))
    }.flatten
  }


  private def hasTwins(tileSet: TileSet) =
    tileSet.tocs.forall(_._2 >= 2)

  private def hasChowsOrTwins(computer: FindAllAndReduce) =
    List(computer.chows, computer.pungs, computer.duis).exists(_.size != 0)
}

object FindAllAndReduce {

  /**
   * Factory to create a FindAllAndReduce given a list of tiles
   * @param tiles list of tiles to compute
   * @return a FigureComputer
   */
  def apply(tiles: List[Tile]): FindAllAndReduce = FindAllAndReduce(TileSet(tiles))

  /**
   * Search for the longest suits.
   * @param tiles where to find suits.
   * @return the list of the longest suits
   */
  def findSuits(tiles: TileSet): List[Suit] = {

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
  def sublistsOf(suitSize: Int, suits: List[Suit]): List[Suit] = {
    require(suitSize > 0)

    def listsOf(suitSize: Int, suit: Suit): List[Suit] = {
      FindAllAndReduce.findSubSuitsIndices(suit.size, suitSize).map {
        indices: List[Int] =>
          indices.map(i => suit(i))
      }
    }

    suits.map(listsOf(suitSize, _)).flatten
  }


  /**
   * Computes all fixed `length` sub-suit indices of a suit of tiles which length is
   * `suitSize`.
   * For instance `findSubSuitsIndices(6,3) == List(List(0,1,2),List(1,2,3),List(2,3,4),List(3,4,5),List(4,5,6))`
   * @param suitSize The list where to compute sub-lists indices from
   * @param length The fixed length of each sub-suit
   * @return a list of list of indices. Indices are sorted ascending and lists too.
   *
   **/
  def findSubSuitsIndices(suitSize: Int, length: Int): List[List[Int]] = {
    val nbElements = suitSize - length + 1
    (for {i <- 0 until nbElements} yield (for {j <- i until i + length} yield j).toList).toList
  }
}