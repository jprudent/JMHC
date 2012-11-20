import scala.math.Ordering

object Chow {

  sealed abstract class Family extends Ordering[Family] {
  	def name : String
  	def compare(f1:Family,f2:Family) = name.compareTo(f2.name)
  }
  case class Bamboo extends Family {
  	override val name = "Bamboo"
  }
  case class Stone extends Family
  case class Character extends Family

  class Tile(val family: Family, val value: Int) extends Ordering[Tile] {
    /* return true if family is same */
    def sameFamily(tile: Tile) = tile.family == family

    /* return true if the `tile` is the previous one in the same family
	     example: Tile("bamboo",1).isPreviousOf(Tile("bamboo",2)) should be true
	  */
    def previousOf(tile: Tile) = sameFamily(tile) && tile.value == value + 1

    /*
    	First criteria of compare is the family, then the value
    */
    def compare(t1: Tile, t2: Tile) =
      t1.family.compare(t2.family) * t1.value.compare(t2.value).abs
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
  };import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(1942); 

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
    } yield Chow(a, b, c)).toList sorted (Chow);System.out.println("""findChows: (tiles: Set[Chow.Tile])List[Chow.Chow]""");$skip(345); val res$0 = 

  findChows(
    Set(
      Tile("bamboo", 1),
      Tile("bamboo", 2),
      Tile("bamboo", 3),
      Tile("bamboo", 4),
      Tile("bamboo", 5),
      Tile("bamboo", 6),
      Tile("bamboo", 7),
      Tile("bamboo", 8),
      Tile("bamboo", 9),
      Tile("stone", 1),
      Tile("stone", 2),
      Tile("stone", 3),
      Tile("stone", 4)));System.out.println("""res0: <error> = """ + $show(res$0))}

}