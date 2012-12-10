package org.liprudent.majiang

import tiles.Tile


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
    override def compare(x: Figure, y: Figure): Int = {
      x match {
        case x: Chow => y match {
          case y: Pung => 1
          case y: Chow => OrdChow.compare(x, y)
          case y: Dui => -1
        }
        case x: Pung => y match {
          case y: Pung => PungProperties.compare(x, y)
          case y: Chow => -1
          case y: Dui => -1
        }
        case x: Dui => y match {
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

}