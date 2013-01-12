package org.liprudent.majiang

import tiles._


package object figures {


  type Suit = List[Tile]

  sealed trait FigureProperties {

    //number of tiles in this figure
    val size: Int

    //ordering
    val order: Int

    def compare(x: Figure, y: Figure): Int

  }

  /**
   * A figure is a '''representation''' of a valid tile arrangement.
   */
  sealed trait Figure {

    //TODO properties is an invariant, have to find a "trick" to express that
    val properties: FigureProperties

    /**
     *
     * @return All tiles composing this figure
     */
    def toTiles: List[Tile]

  }

  /**
   * An ordering implementation where `List[Figure]` is ordered this way:
   * $ -[[org.liprudent.majiang.figures.ThirteenOrphans]]
   * $ -[[org.liprudent.majiang.figures.SomeKnittedWithSomeDragons]]
   * $ -[[org.liprudent.majiang.figures.Knitted]]
   * $ -[[org.liprudent.majiang.figures.Kong]]
   * $ -[[org.liprudent.majiang.figures.Pung]]
   * $ -[[org.liprudent.majiang.figures.Chow]]
   * $ -[[org.liprudent.majiang.figures.Dui]]
   * $ -[[org.liprudent.majiang.figures.Bonus]]
   * $ -[[org.liprudent.majiang.figures.SingleTile]]
   *
   * @note This actual responsability is holded by figures themself throught the property `order`
   */
  implicit object OrdFigure extends Ordering[Figure] {

    override def compare(f1: Figure, f2: Figure): Int = {

      val diff = f1.properties.order - f2.properties.order

      diff match {
        case 0 => f1.properties.compare(f1, f2)
        case n => n
      }

    }
  }


  /**
   * Thirteen orphans is a special figure of 14 tiles matching this exact pattern:
   * b1,b9,c1,c9,s1,s9,we,wn,ww,ws,dr,dg,dw + another one of these
   *
   * @param extraTile is the extra tile to form a pair. Must be terminal or honor.
   */
  case class ThirteenOrphans(extraTile: Tile) extends Figure {

    require(extraTile.isTerminalOrHonor)

    val properties = ThirteenOrphansProperties

    override def toTiles = (extraTile :: properties.fixedTiles).sorted
  }

  // TODO centralize in companion
  object ThirteenOrphansProperties extends FigureProperties {

    val size = 14

    val order = 0

    def compare(x: Figure, y: Figure) =
      Tile.ord.compare(x.asInstanceOf[ThirteenOrphans].extraTile, y.asInstanceOf[ThirteenOrphans].extraTile)

    val fixedTiles = List(
      Tile.b1, Tile.b9,
      Tile.c1, Tile.c9,
      Tile.s1, Tile.s9,
      Tile.we, Tile.wn, Tile.ww, Tile.ws,
      Tile.dr, Tile.dg, Tile.dw).sorted
  }


  /**
   * Special figure of 14 tiles matching complete or partial knitted tiles with distinct honors.
   *
   * @param knitted The complete or partial knitted tiles. Must be straight tiles
   * @param honors The list of distincts honors.
   * @see org.liprudent.majiang.figures.Knitted
   *
   */
  case class SomeKnittedWithSomeDragons(knitted: List[Tile], honors: List[Tile]) extends Figure {

    require(knitted.size + honors.size == 14, "must be 14 tiles")

    require(knitted.forall(!_.isHonor), "wrong kind of tiles for knitted tiles")
    require(knitted == knitted.sorted, "sorted required for knitted tiles")
    require(TileSet(knitted).tocs.size == knitted.size, "no pair, no pung allowed in knitted tiles")

    require(honors.forall(_.isHonor), "wrong kind of tiles for honors")
    require(honors == honors.sorted, "sorted required for honors")
    require(TileSet(honors).tocs.size == honors.size, "no pair, no pung allowed in honors")

    val properties = SomeKnittedWithSomeDragonsProperties

    lazy val groupedKnitted: List[List[Tile]] = knitted.groupBy(_.family).values.toList

    override def toTiles = honors ::: knitted

  }

  // TODO centralize in companion
  object SomeKnittedWithSomeDragonsProperties extends FigureProperties {

    val size = 14

    val order = 10

    def compare(x: Figure, y: Figure) =
    // the more dragons you have, the strongest you are
      x.asInstanceOf[SomeKnittedWithSomeDragons].honors.size - y.asInstanceOf[SomeKnittedWithSomeDragons].honors.size
  }


  /**
   * Knitted tiles is a special figure of 9 tiles matching the pattern:<br>
   * 1, 4, 7 in family one<br>
   * 2, 5, 8 in family two<br>
   * 3, 6, 9 in family three<br>
   * For instance : b1, c2, s3, b4, c5, s6, b7, c8, s9
   *
   * @param fam147 Family of 1, 4, 7
   * @param fam258 Family of 2, 5, 8
   * @param fam369 Family of 3, 6, 9
   */
  case class Knitted(fam147: StraightFamily, fam258: StraightFamily, fam369: StraightFamily) extends Figure {

    require(fam147 != fam258 && fam147 != fam369 && fam258 != fam369)

    val properties = KnittedProperties

    override def toTiles = List(
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

  // TODO centralize in companion
  object KnittedProperties extends FigureProperties {

    val size = 9

    val order = 20

    def compare(knitted1: Figure, knitted2: Figure) =
      knitted2.asInstanceOf[Knitted].fam147.order - knitted1.asInstanceOf[Knitted].fam147.order
  }

  abstract class PungLike(val tile: Tile) extends Figure {

    def sameFamily(p: PungLike): Boolean = tile.sameFamily(p.tile)

    def sameFamily(x: PungLike, y: PungLike): Boolean =
      sameFamily(x) && sameFamily(y)

    def sameValue(p: PungLike) = tile.sameValue(p.tile)

    override def toTiles = (0 until properties.size map (i => tile)).toList
  }

  /**
   * In various card games, particulary poker, a ''kong'' is called ''four of a kind''
   *
   * In neutral language that would be called ''four times the same tile''
   *
   * @param tile The tile four times
   *
   * @note Kong extends `PungLike` because a kong is mostly seen as pung.
   *       The difference resides when counting points.
   */
  case class Kong(override val tile: Tile) extends PungLike(tile) {

    val properties = KongProperties

  }

  // TODO centralize in companion
  object KongProperties extends FigureProperties {

    val size = 4

    val order = 30

    def compare(kong1: Figure, kong2: Figure) =
      Tile.ord.compare(kong1.asInstanceOf[Kong].tile, kong2.asInstanceOf[Kong].tile)
  }


  /**
   * In various card games, particulary poker, a ''pung'' is called ''three of a kind''
   *
   * In neutral language that would be called ''three times the same tile''
   *
   * @param tile The tile three times
   */
  case class Pung(override val tile: Tile) extends PungLike(tile) {

    val properties = PungProperties

  }

  // TODO centralize in companion
  object PungProperties extends FigureProperties {

    val size = 3

    val order = 40

    def compare(pung1: Figure, pung2: Figure) =
      Tile.ord.compare(pung1.asInstanceOf[Pung].tile, pung2.asInstanceOf[Pung].tile)
  }


  /**
   * A Chow is a straight of 3 consecutive tiles of the same family.
   *
   * For example: b1,b2,b3
   *
   * @param t1 First tile of the straight
   * @param t2 Second tile of the straight
   * @param t3 Third tile of the straight
   *
   *           TODO simplify construction with the first tile only. Other tiles can be implied.
   */
  case class Chow(t1: Tile, t2: Tile, t3: Tile) extends Figure {

    require(t1.previousOf(t2) && t2.previousOf(t3))

    def this(xs: List[Tile]) {
      this(xs(0), xs(1), xs(2))
      require(xs.length == 3)
    }

    val properties = Chow

    override def toTiles = List(t1, t2, t3)

    def sameValues(y: Chow) =
      y.t1.value == t1.value

    def isConsequitive(y: Chow) =
      y.t1.value == t1.value + 3

    def sameFamily(y: Chow): Boolean =
      t1.sameFamily(y.t1)

    def sameFamily(y: Chow, z: Chow): Boolean =
      sameFamily(y) && sameFamily(z)

    lazy val isStartingChow = t1.isFirst

    lazy val isMiddleChow = t1.value == 4

    lazy val isEndingChow = t3.isLast

    lazy val family = t1.family
  }


  //TODO centralize in the companion object
  object OrdChow extends Ordering[Chow] {

    override def compare(chow1: Chow, chow2: Chow) = Tile.ord.compare(chow1.t1, chow2.t1)

  }

  object Chow extends FigureProperties {

    val size = 3

    val order = 50

    def compare(x: Figure, y: Figure) = OrdChow.compare(x.asInstanceOf[Chow], y.asInstanceOf[Chow])

    def apply(t: Tile) = {
      require(t.isStraight, "a chow can only be made of Straight Tiles")
      require(t.value <= 7, "last tile of a chow must be lesser than 9")
      new Chow(t, Tile(t.family, t.value + 1), Tile(t.family, t.value + 2))
    }

  }


  /**
   * Dui means Pair in Mandarin Chinese.
   * In neutral language, we would say ''two times the same card''
   *
   * @param tile the tile two times
   *
   * @note Why I didn't call this class Pair ? Since Pair is also a classname in scala library, I had to find a new
   *       name to avoid qualifying classname.
   */
  case class Dui(tile: Tile) extends Figure {

    val properties = DuiProperties

    lazy val family = tile.family

    lazy val value = tile.value

    override def toTiles = List(tile, tile)

  }

  // TODO centralize in companion
  object DuiProperties extends FigureProperties {

    val size = 2

    val order = 60

    def compare(dui1: Figure, dui2: Figure) =
      Tile.ord.compare(dui1.asInstanceOf[Dui].tile, dui2.asInstanceOf[Dui].tile)

  }

  /**
   * Bonus is a set of bonus tiles (flowers and seasons).
   *
   * @param bonus List of flowers and seasons
   *
   * @see [[org.liprudent.majiang.tiles.BonusFamily]]
   */
  case class Bonus(bonus: List[Tile]) extends Figure {

    require(bonus.forall(_.isBonus), "Only bonus tiles are accepted")
    require(bonus == bonus.sorted, "Tiles should be sorted")

    // TODO centralize in companion
    val properties = new FigureProperties {
      val size: Int = bonus.size
      val order = 70

      override def compare(x: Figure, y: Figure) =
        x.asInstanceOf[Bonus].bonus.size - y.asInstanceOf[Bonus].bonus.size
    }

    override def toTiles = bonus

  }

  //TODO this is used to hold the result of LastTile combination
  //TODO the proper way to implement it is to create 2 bounded contexts:
  //TODO one for input and result of mahjong hand finder
  //TODO another for point computer results
  case class SingleTile(t: Tile) extends Figure {
    val properties = new FigureProperties {
      val size: Int = 1
      val order = 80

      def compare(x: Figure, y: Figure) =
        Tile.ord.compare(x.asInstanceOf[SingleTile].t, y.asInstanceOf[SingleTile].t)
    }

    override def toTiles = List(t)
  }

}