import scala.math.Ordering

object Tiles {

  sealed abstract class Family extends Ordering[Family] {
    def name: String

    def compare(f1: Family, f2: Family) = name.compareTo(f2.name)
  }

  case class Bamboo extends Family {
    override val name = "Bamboo"
  }

  case class Stone extends Family {
    override val name = "Stone"
  }

  case class Character extends Family {
    override val name = "Character"
  }

  class Tile(val family: Family, val value: Int) extends Ordering[Tile] {

    // a tile value should be between 1 and 9
    require(value >= 1 && value <= 9)

    /**
     * return true if family is same
     */
    def sameFamily(tile: Tile) = tile.family == family

    /**
     * return true if the `tile` is the previous one in the same family
     * example: Tile(Bamboo,1).isPreviousOf(Tile(Bamboo,2)) should be true
     */
    def previousOf(tile: Tile) = sameFamily(tile) && tile.value == value + 1

    /**
     * First criteria of compare is the family, then the value
     */
    def compare(t1: Tile, t2: Tile) =
      t1.family.compare(t1.family, t2.family) * t1.value.compare(t2.value).abs

    override def toString = value + "-" + family.name.substring(0, 3)
  }

  object Tile {

    def apply(family: Family, value: Int) = {
      new Tile(family, value)
    }

  }

  case class Chow(t1: Tile, t2: Tile, t3: Tile)
    extends Tuple3[Tile, Tile, Tile](t1, t2, t3)

  /* FIXME : I would like to be able to sort a List[Chow]. Why the method
	`sorted` doesn't work out of the box and I do have to create a companion
	object ? */

  object Chow extends Ordering[Chow] {
    def compare(chow1: Chow, chow2: Chow) = chow1.t1.compare(chow1.t1, chow2.t1)
  };

  def main(args: Array[String]) = $execute {
    ;
    $skip(2164);

    /**
     * return an ordered list of possible chows without duplicates
     *
     * if hand has 1 2 3 bamboos twice, the result will contains 1 2 3 bamboos
     * only once whereas a valid hand may contains 1 2 3 bamboos twice
     */
    def findChows(tiles: Set[Tile]): List[Chow] =
      (for {
        a <- tiles
        b <- tiles.dropWhile(_ == a) if a.previousOf(b)
        c <- tiles.dropWhile(_ == b) if b.previousOf(c)
      } yield Chow(a, b, c)).toList sorted (Chow);

    findChows(
      Set(
        Tile(Bamboo(), 1),
        Tile(Bamboo(), 2),
        Tile(Bamboo(), 3),
        Tile(Bamboo(), 4),
        Tile(Bamboo(), 5),
        Tile(Bamboo(), 6),
        Tile(Bamboo(), 7),
        Tile(Bamboo(), 8),
        Tile(Bamboo(), 9),
        Tile(Stone(), 1),
        Tile(Stone(), 2),
        Tile(Stone(), 3),
        Tile(Stone(), 4)));

  }