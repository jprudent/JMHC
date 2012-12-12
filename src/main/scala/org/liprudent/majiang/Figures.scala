package org.liprudent.majiang

import tiles.{TileSet, SuitFamily, Tile}


package object figures {


  type Suit = List[Tile]

  sealed trait FigureProperties {
    val size: Int
  }

  sealed trait Figure {
    val properties: FigureProperties

    def asList: List[Tile]
  }

  implicit object OrdFigure extends Ordering[Figure] {
    //TODO need to understand why result is reversed
    override def compare(x: Figure, y: Figure): Int = {
      x match {

        case x: SomeKnittedWithSomeDragons => y match {
          case y: SomeKnittedWithSomeDragons => SomeKnittedWithSomeDragonsProperties.compare(x, y)
          case _ => -1
        }

        case x: Knitted => y match {
          case y: SomeKnittedWithSomeDragons => 1
          case y: Knitted => KnittedProperties.compare(x, y)
          case y: Pung => -1
          case y: Chow => -1
          case y: Dui => -1
        }
        case x: Chow => y match {
          case y: SomeKnittedWithSomeDragons => 1
          case y: Knitted => 1
          case y: Pung => 1
          case y: Chow => OrdChow.compare(x, y)
          case y: Dui => -1
        }
        case x: Pung => y match {
          case y: SomeKnittedWithSomeDragons => 1
          case y: Knitted => 1
          case y: Pung => PungProperties.compare(x, y)
          case y: Chow => -1
          case y: Dui => -1
        }
        case x: Dui => y match {
          case y: SomeKnittedWithSomeDragons => 1
          case y: Knitted => 1
          case y: Pung => 1
          case y: Chow => 1
          case y: Dui => DuiProperties.compare(x, y)
        }
      }
    }
  }


  case class Chow(t1: Tile, t2: Tile, t3: Tile) extends Figure {

    require(t1.previousOf(t2) && t2.previousOf(t3))

    def this(xs: List[Tile]) {
      this(xs(0), xs(1), xs(2))
      require(xs.length == 3)
    }

    val properties = Chow

    override def asList = List(t1, t2, t3)
  }

  object OrdChow extends Ordering[Chow] {
    override def compare(chow1: Chow, chow2: Chow) = Tile.ord.compare(chow1.t1, chow2.t1)
  }

  object Chow extends FigureProperties {
    val size = 3
  }


  case class Dui(t: Tile) extends Figure {
    val properties = DuiProperties

    override def asList = List(t, t)
  }


  object DuiProperties extends Ordering[Dui] with FigureProperties {

    val size = 2

    def compare(dui1: Dui, dui2: Dui) = Tile.ord.compare(dui1.t, dui2.t)

  }


  case class Pung(t: Tile) extends Figure {
    val properties = PungProperties

    override def asList = List(t, t, t)
  }


  object PungProperties extends Ordering[Pung] with FigureProperties {

    val size = 3

    def compare(pung1: Pung, pung2: Pung) = Tile.ord.compare(pung1.t, pung2.t)
  }

  case class Knitted(fam147: SuitFamily, fam258: SuitFamily, fam369: SuitFamily) extends Figure {

    require(fam147 != fam258 && fam147 != fam369 && fam258 != fam369)

    val properties = KnittedProperties

    override def asList = List(
      Tile(fam147, 1),
      Tile(fam147, 4),
      Tile(fam147, 7),
      Tile(fam258, 2),
      Tile(fam258, 5),
      Tile(fam258, 8),
      Tile(fam369, 3),
      Tile(fam369, 6),
      Tile(fam369, 9)
    )
  }

  object KnittedProperties extends Ordering[Knitted] with FigureProperties {

    val size = 9

    def compare(knitted1: Knitted, knitted2: Knitted) =
      knitted2.fam147.order - knitted1.fam147.order
  }

  case class SomeKnittedWithSomeDragons(knitted: List[Tile], dragons: List[Tile]) extends Figure {
    require(knitted.forall(_.family.isInstanceOf[SuitFamily]), "wrong kind")
    require(dragons.forall(!_.family.isInstanceOf[SuitFamily]), "wrong kind")
    require(knitted == knitted.sorted, "sorted required")
    require(dragons == dragons.sorted, "sorted required")
    require(TileSet(knitted).tocs.size == knitted.size, "no pair, no pung allowed")
    require(TileSet(dragons).tocs.size == dragons.size, "no pair, no pung allowed")

    val properties = SomeKnittedWithSomeDragonsProperties

    override def asList = dragons ::: knitted

  }

  object SomeKnittedWithSomeDragonsProperties extends Ordering[SomeKnittedWithSomeDragons] with FigureProperties {

    val size = 14

    def compare(x: SomeKnittedWithSomeDragons, y: SomeKnittedWithSomeDragons) =
    // the more dragons you have, the strongest you are
      x.dragons.size - y.dragons.size
  }

}