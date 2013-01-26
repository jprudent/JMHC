package org.liprudent.majiang.computer

import org.liprudent.majiang.tiles._
import org.liprudent.majiang.figures._
import scala.Some
import org.liprudent.majiang.figures.SingleTile
import org.liprudent.majiang.figures.Dui
import java.io.FileWriter

case class GraphBuilder(filename: String) {


  val f = new FileWriter(filename, true)

  def header() = print( """
                          |
                          |graph G {
                          |   edge [len=2]
                          |
                        """.stripMargin)

  def footer = print(
    """
      |
      |}
      |
    """.stripMargin)

  def node(node1: String, node2: String, edge: String) = {
    print(
      """%s -- %s [label="%s"]
        |
      """.stripMargin.format(node1, node2, edge))
    //f.close()
  }

  private def print(s: String) = {
    f.write(s)
    f.flush()
  }

  private def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try {
      op(p)
    } finally {
      p.close()
    }
  }
}

//TODO créer un objet
case class TreeBuilder(tileset: TileSet) extends TilesToFiguresService {

  val graph = GraphBuilder("/tmp/foo")

  var cpt = 0

  /**
   * Find all possible combinations of tiles
   *
   * @return Set of all figures combinations
   */
  def allFiguresCombinations: Set[Types.Figures] = {
    val tiles: List[Tile] = tileset.toTiles.toSet.toList
    tiles.par.map(t => discoverBranch(t, tileset.toTiles, Nil)).flatten.toList.toSet
  }

  //  private def discoverTree(t:Tile,remainingTile:List[Tile],result:Types.Figures):Set[Types.Figures] = {
  //
  //  }
  private def discoverBranch(t: Tile, remainingTile: List[Tile], result: Types.Figures): Seq[Types.Figures] = {

    cpt = cpt + 1
    //println("---\ncpt=%d\ntile=%s\nremaining=%s\nresult=%s".format(cpt,t,remainingTile,result))
    val newResult = addLeaf(t, result)

    newResult match {
      case Nil => Nil
      case _ => {

        val allButT = TreeBuilder.removeFirst(remainingTile) {
          _ == t
        }
        assert(allButT.size == remainingTile.size - 1)

        allButT match {
          case Nil => {
            newResult.head match {
              case f: PartialFigure => Nil
              case f: SingleTile => Nil
              case _ => List(newResult.reverse)
            }
          }
          case _ => {
            val tiles = allButT.toSet.toList
            tiles.map {
              next =>
                discoverBranch(next, allButT, newResult)
            }.flatten
          }
        }
      }
    }
  }

  /**
   *
   * @param tile
   * @param result
   * @return Nil if the construction is invalid. The new result otherwise.
   */
  private def addLeaf(tile: Tile, result: Types.Figures): Types.Figures = {

    //require(result == result.sorted(OrdFigure).reverse,result)
    require(!tile.isFlower)

    result match {
      case Nil => List(SingleTile(tile))
      case leaf :: validBranch => {
        val enhancement: Types.Figures = enhanceFigure(tile, leaf, validBranch)
        enhancement match {
          case Nil => {
            //println("abandoning tile = %s , result = %s".format(tile,result))
            Nil
          }
          case enhanced :: Nil => enhanced :: validBranch
          case newFigure :: leaf => newFigure :: result
        }
      }
    }
  }

  /**
   *
   * @param tile Tile to add to figure
   * @param figure The figure to be enhanced
   * @param tail The other figures
   * @return - List(SingleTile(tile),figure) if we can't add tile to figure
   *         - List(Figure()) if tile can be added to figure
   *         - Nil if construction is invalid
   *         Construction is invalid if :
   *         the enhancedFigure < tail.head
   *         figure is a PartialFigure and can't be enhanced
   */
  private def enhanceFigure(tile: Tile, figure: Figure, tail: Types.Figures): Types.Figures = {
    val enhanced = FigureEnhancerService.addTile(figure, tile)

    enhanced match {
      case None => figure match {
        case f: PartialFigure => Nil
        case f: SingleTile => Nil
        case _ => List(SingleTile(tile), figure)
      }
      case Some(enhanced) => enhanced match {
        case enhanced: PartialFigure => List(enhanced)
        case enhanced => {
          tail match {
            case Nil => List(enhanced)
            case _ =>
              if (OrdFigure.lt(enhanced, tail.head) || tail.head.isInstanceOf[Dui]) {
                Nil
              } else {
                List(enhanced)
              }
          }
        }
      }
    }
  }
}

object TreeBuilder {

  private def removeFirst[T](list: List[T])(pred: (T) => Boolean): List[T] = {
    val (before, atAndAfter) = list span (x => !pred(x))
    before ::: atAndAfter.drop(1)
  }
}


abstract class PartialNature {
  def isValid(tiles: List[Tile]): Boolean

  def toConcrete(tiles: List[Tile]): Option[Figure] = {
    isValid(tiles) match {
      case true => optionalConcrete(tiles)
      case false => None
    }
  }

  protected def optionalConcrete(tiles: List[Tile]): Option[Figure]
}


//TODO these objects should be case class
//object PartialDui extends PartialNature {
//  def isValid(tiles: List[Tile]) = {
//    tiles.size == 1
//  }
//
//  protected def optionalConcrete(tiles: List[Tile]) = None
//}
object PartialChow extends PartialNature {

  def isValid(tiles: List[Tile]) = {

    require(tiles == tiles.sorted(Tile.ord))

    require(tiles.size > 1)

    //all are straight
    tiles.forall(_.isStraight) &&
      //only one family
      tiles.map(_.family).toSet.size == 1 &&
      //tiles are shifted by one
      isSuit(tiles)
  }

  protected def optionalConcrete(tiles: List[Tile]) = {
    tiles.size == 3 match {
      case true => Some(Chow(tiles.head))
      case false => None
    }

  }

  private def isSuit(tiles: List[Tile]): Boolean = {
    tiles match {
      case h :: t if t != Nil => h.previousOf(t.head) && isSuit(t)
      case _ => true
    }
  }
}

object PartialKnitted extends PartialNature {

  def isValid(tiles: List[Tile]) = {
    require(tiles == tiles.sorted(Tile.ord))

    val byFamily = tiles.groupBy(_.family)

    //only straight
    tiles.forall(_.isStraight) &&
      //all unique
      TileSet(tiles).isAllUnique &&
      //shifted by 3
      byFamily.forall {
        case (family, tiles) => arePurelyShifed(tiles)
      } &&
      //and in proper sets of values
      areInProperGroups(byFamily.values.toList, Set(Set(1, 4, 7), Set(2, 5, 8), Set(3, 6, 9)))
  }

  protected def optionalConcrete(tiles: List[Tile]) = {
    tiles.size == 9 match {
      case true => {
        //tile.value % 3 tells us which group (147 : % = 1, 258 : % = 2, 369 : % = 0) the tile belongs to
        val byGroup = tiles.groupBy(_.value % 3)

        //I'm sure the three families are present
        assert(byGroup.size == 3)

        def familyFor(modulo: Int) = byGroup.get(modulo).get.head.family.asInstanceOf[StraightFamily]

        Some(Knitted(familyFor(1), familyFor(2), familyFor(0)))
      }
      case false => None
    }
  }

  def arePurelyShifed(tiles: List[Tile]): Boolean = {

    require(tiles == tiles.sorted(Tile.ord))

    tiles match {
      case h :: t if t != Nil => (t.head.value - h.value) % 3 == 0 && arePurelyShifed(t)
      case _ => true
    }
  }

  def areInProperGroups(tilesByFamily: List[List[Tile]], groups: Set[Set[Int]]): Boolean = {

    require(tilesByFamily.size <= groups.size, "tiles:%s \ngroups:%s".format(tilesByFamily, groups))

    tilesByFamily match {
      case Nil => true
      case tiles :: t => {
        val tilesAsSet = tiles.map(_.value).toSet
        val in = groups.filter {
          group => group.intersect(tilesAsSet).size != 0
        }
        in.size == 1 && areInProperGroups(t, groups - in.head)
      }
    }
  }
}

object PartialSomeKnittedWithSomeDragons extends PartialNature {
  def isValid(tiles: List[Tile]) = {
    val (straights, honors) = splitStaightDragons(tiles)
    PartialKnitted.isValid(straights) &&
      honors != Nil &&
      TileSet(honors).isAllUnique
  }

  protected def optionalConcrete(tiles: List[Tile]) =
    tiles.size == 14 match {
      case true => {
        val (straights, honors) = splitStaightDragons(tiles)
        Some(SomeKnittedWithSomeDragons(straights, honors))
      }
      case false => None
    }

  private def splitStaightDragons(tiles: List[Tile]) = tiles.partition(_.isStraight)
}

object PartialThirteenOrphans extends PartialNature {

  def isValid(tiles: List[Tile]) = {
    val (straights, honors) = tiles.partition(_.isStraight)
    // maximum one pair
    tiles.size - tiles.toSet.size <= 1 &&
      // only 1 and 9
      straights.groupBy(_.family).values.forall(_.forall(_.isTerminal))
  }

  protected def optionalConcrete(tiles: List[Tile]) = {
    tiles.size == 14 match {
      case true => {
        val extraTile = TileSet(tiles).allPairs.head //I know this will be found
        Some(ThirteenOrphans(extraTile))
      }
      case false => None
    }
  }
}

/**
 * @param tiles Tiles of this partial Figure
 * @param natures define this partial figure is partial of what
 */
case class PartialFigure(tiles: List[Tile], natures: Set[PartialNature]) extends Partial(tiles) {
  require(tiles.size >= 2)
  require(tiles == tiles.sorted(Tile.ord))
  require(natures != Nil)

  require(natures.forall(_.isValid(tiles)))
}

object PartialFigure {
  val allNatures = Set(PartialChow, PartialKnitted, PartialSomeKnittedWithSomeDragons, PartialThirteenOrphans)

  def apply(tiles: List[Tile]): Option[Figure] = {
    //TODO allNatures.isValid is computed twice
    val concreteFigures: Set[Option[Figure]] = PartialFigure.allNatures.map(_.toConcrete(tiles)).filterNot(_ == None)
    assert(concreteFigures.size <= 1, "A partial figure should generate only one concrete figure")

    if (!concreteFigures.isEmpty) concreteFigures.head //there is only one element
    else {
      val natures = PartialFigure.allNatures.filter(_.isValid(tiles))
      if (natures.isEmpty) None
      else Some(new PartialFigure(tiles, natures))
    }
  }
}

object FigureEnhancerService {

  /**
   * Build a new figure with new tile `t`
   * @param figure The figure underlying the new figure
   * @param tile the new tile to build the new Figure against
   * @return Some if it's possible, None otherwise
   * @example Dui(b2).addTile(b2) === Some(Pung(b2))
   * @example Dui(b2).addTile(b3) === None
   *
   */
  def addTile(figure: Figure, tile: Tile): Option[Figure] = {
    figure match {
      case f: SingleTile => addTile(f, tile)
      case f: Dui => addTile(f, tile)
      case f: Chow => None
      case f: Pung => None
      case f: Knitted => addTile(f, tile)
      case f: SomeKnittedWithSomeDragons => None
      case f: ThirteenOrphans => None
      case f: PartialFigure => PartialFigure(tiles(figure, tile))
      case _ => throw new IllegalArgumentException("unknown figure " + figure)
    }
  }

  private def addTile(f: Knitted, t: Tile): Option[Figure] = {
    PartialFigure(tiles(f, t))
  }

  private def addTile(f: Dui, t: Tile): Option[Figure] = {
    if (t == f.tile) Some(Pung(t))
    else PartialFigure(tiles(f, t))
  }

  private def addTile(f: SingleTile, t: Tile): Option[Figure] = {
    if (f.tile == t) Some(Dui(t))
    else PartialFigure(tiles(f, t))
  }

  private def tiles(f: Figure, t: Tile) =
    (t :: f.toTiles).sorted(Tile.ord)
}