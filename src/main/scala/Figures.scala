package org.liprudent.majiang

import tiles.Tile

package object figures {


  type Suit = List[Tile]

  sealed trait FigureProperties {
    val size: Int
  }

  sealed trait Figure {
    val properties: FigureProperties
  }

  implicit object OrdFigure extends Ordering[Figure] {
    override def compare(f1: Figure, f2: Figure) = {
      f1 match {
        case chow1: Chow => f2 match {
          case chow2: Chow => OrdChow.compare(chow1, chow2)
        }
        case _ => throw new Exception("Not Implemented")
      }
    }
  }


  case class Chow(t1: Tile, t2: Tile, t3: Tile) extends Figure {

    def this(xs: List[Tile]) {
      this(xs(0), xs(1), xs(2))
      require(xs.length == 3)
    }

    val properties = Chow
  }

  object OrdChow extends Ordering[Chow] {
    override def compare(chow1: Chow, chow2: Chow) = Tile.ord.compare(chow1.t1, chow2.t1)
  }

  object Chow extends FigureProperties {
    val size = 3
  }


  case class Dui(t: Tile) extends Figure {
    val properties = DuiProperties
  }


  object DuiProperties extends Ordering[Dui] with FigureProperties {

    val size = 2

    def compare(dui1: Dui, dui2: Dui) = Tile.ord.compare(dui1.t, dui2.t)

  }


  case class Pung(t: Tile) extends Figure {
    val properties = PungProperties
  }

  object PungProperties extends Ordering[Pung] with FigureProperties {

    val size = 3

    def compare(pung1: Pung, pung2: Pung) = Tile.ord.compare(pung1.t, pung2.t)
  }

}