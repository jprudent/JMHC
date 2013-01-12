package org.liprudent.majiang.mahjong

import org.liprudent.majiang.tiles._
import Tile._
import org.liprudent.majiang.figures._
import org.liprudent.majiang.HuFinder
import org.liprudent.majiang.figures.Pung
import org.liprudent.majiang.figures.Bonus
import org.liprudent.majiang.figures.Dui
import org.liprudent.majiang.tiles.ContextualTile
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
 * Acceptance Tests
 *
 * here is a template :
 * {{{
    test(
    """use case :
      |verif : Mahjong Friends
      |tricky part:
    """.stripMargin) {
    val givenClosed = TileSet(List())
    val givenMelded: List[Figure] = List()
    val givenContextualTile: ContextualTile = ContextualTile(s3, Discarded, NotLastTile)
    val givenConcealedKongs = List()
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List()
    val allFigures = (thenClosed ::: givenMelded ::: givenConcealedKongs).sorted(OrdFigure)
    val thenPoints =
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (List(),),
        (List(),),
        (List(),),
        (List(),),
        (List(),)
      )

    test(givenClosed, givenMelded, givenConcealedKongs, givenContextualTile, givenBonus, givenContext, thenClosed,
thenCombinations, thenPoints)

  }
 * }}}
 * TODO:
 * $ - Two or more TileHogs
 * $ - 1 concealed pung, and finish with the third tile of a second pung => Two Concealed Pungs is not scored
 * $ - Two pure shifted chows
 * $ - One concealed kong and one melded kong should score 6
 * $ - robbing the kong and unique wait ?
 * $ - robbing the kong and last tile ?
 * $ - out on last tile claim and last tile draw ? Should I score both ?
 */
@RunWith(classOf[JUnitRunner])
class MahjongFriendUseCaseSuite extends FunSuite {

  test(
    """"case MeldedHand AllChows MixedDoubleChows
      |verif: dubious on One Voided Suit
    """.stripMargin) {
    val givenClosed = TileSet(List(b3, b3))
    val givenMelded = List(Chow(b1, b2, b3), Chow(b5, b6, b7), Chow(c5, c6, c7), Chow(c7, c8, c9))
    val givenContextualTile = ContextualTile(b3, Discarded, NotLastTile)
    val givenContext = PlayerContext(WestWind, EastWind)

    val thenClosed = List(Dui(b3))
    val thenCombinations = List(
      (List(Chow(b1, b2, b3), Chow(b5, b6, b7), Chow(c5, c6, c7), Chow(c7, c8, c9), Dui(b3)), MeldedHand),
      (List(Chow(b1, b2, b3), Chow(b5, b6, b7), Chow(c5, c6, c7), Chow(c7, c8, c9)), AllChows),
      (List(Chow(b5, b6, b7), Chow(c5, c6, c7)), MixedDoubleChows),
      (List(Chow(b1, b2, b3), Chow(b5, b6, b7), Chow(c5, c6, c7), Chow(c7, c8, c9), Dui(b3)), OneVoidedSuit)
      //implied by MeldedHand (List(Dui(b3)), SingleWait)
    )

    test(givenClosed, givenMelded, givenContextualTile, givenContext, thenClosed, thenCombinations, 10)

  }

  test("case Mixed Triple Chows - All Chows") {
    val givenClosed = TileSet(List(s2, s3, s4, s6, s7, s8, s9, s9))
    val givenContextualTile = ContextualTile(s9, Discarded, NotLastTile)
    val givenMelded = List(Chow(b6, b7, b8), Chow(c6, c7, c8))
    val givenContext = PlayerContext(WestWind, EastWind)

    val thenClosed: List[Figure with Product] = List(Chow(s2, s3, s4), Chow(s6, s7, s8), Dui(s9))
    val thenCombinations: List[(List[Chow], Combination)] = List(
      (List(Chow(b6, b7, b8), Chow(c6, c7, c8), Chow(s6, s7, s8)), MixedTripleChow),
      (List(Chow(b6, b7, b8), Chow(c6, c7, c8), Chow(s2, s3, s4), Chow(s6, s7, s8)), AllChows)
    )

    test(givenClosed, givenMelded, givenContextualTile, givenContext, thenClosed, thenCombinations, 10)
  }



  test("case Upper Four - Flowers - Double Chow - Closed Wait") {

    val givenClosed = TileSet(List(s6, s6, s6, s7, s8, s9, c6, c6))
    val givenMelded: List[Chow] = List(Chow(b6, b7, b8), Chow(c7, c8, c9))
    val givenContextualTile: ContextualTile = ContextualTile(s8, Discarded, NotLastTile)
    val givenBonus: Bonus = Bonus(List(fb, ss, sa))
    val givenContext = PlayerContext(WestWind, EastWind)

    val thenClosed = List(Pung(s6), Chow(s7, s8, s9), Dui(c6))
    val thenCombinations =
      List(
        (List(Pung(s6), Chow(b6, b7, b8), Chow(c7, c8, c9), Chow(s7, s8, s9), Dui(c6)), UpperFour),
        (List(Chow(c7, c8, c9), Chow(s7, s8, s9)), MixedDoubleChows),
        (List(Chow(s7, s8, s9)), ClosedWait),
        (List(Bonus(List(fb, ss, sa))), FlowerTiles)
      )

    test(givenClosed, givenMelded, givenContextualTile, givenBonus, givenContext, thenClosed, thenCombinations, 17)

  }

  test("case All Types - SeatWind - Flower - Single Wait") {

    val givenClosed = TileSet(List(b2, b2, b2, c1, c2, c3, dg, dg))
    val givenMelded = List(Pung(ww), Chow(s3, s4, s5))
    val givenContextualTile: ContextualTile = ContextualTile(dg, Discarded, NotLastTile)
    val givenBonus: Bonus = Bonus(List(fo))
    val givenContext = PlayerContext(WestWind, EastWind)

    val thenClosed = List(Pung(b2), Chow(c1, c2, c3), Dui(dg))
    val thenCombinations =
      List(
        (List(Pung(b2), Pung(ww), Chow(c1, c2, c3), Chow(s3, s4, s5), Dui(dg)), AllTypes),
        (List(Pung(ww)), SeatWind),
        (List(Dui(dg)), SingleWait),
        (List(Bonus(List(fo))), FlowerTiles)
      )

    test(givenClosed, givenMelded, givenContextualTile, givenBonus, givenContext, thenClosed, thenCombinations, 10)

  }

  test("case Knitted Straight - Fully Concealed Hand") {
    val givenClosed = TileSet(List(b1, b4, b7, c2, c5, c8, s3, s6, s9, c8, c8, c8, dr, dr))
    val givenMelded = Nil
    val givenContextualTile: ContextualTile = ContextualTile(b1, SelfDrawn, NotLastTile)
    val givenBonus: Bonus = Bonus(List(fo))
    val givenContext = PlayerContext(WestWind, EastWind)

    val thenClosed = List(Knitted(Bamboo, Character, Stone), Pung(c8), Dui(dr))
    val thenCombinations =
      List(
        (List(Knitted(Bamboo, Character, Stone)), KnittedStraight),
        (List(Knitted(Bamboo, Character, Stone), Pung(c8), Dui(dr)), FullyConcealedHand),
        (List(Knitted(Bamboo, Character, Stone), Pung(c8)), TileHog),
        (List(Bonus(List(fo))), FlowerTiles)
      )

    test(givenClosed, givenMelded, givenContextualTile, givenBonus, givenContext, thenClosed, thenCombinations, 19)

  }

  test("case Knitted Straight - All Types") {

    val givenClosed = TileSet(List(b1, b4, b7, c2, c5, c8, s3, s6, s9, dr, dr))
    val givenMelded = List(Pung(ww))
    val givenContextualTile: ContextualTile = ContextualTile(b1, Discarded, NotLastTile)
    val givenBonus: Bonus = Bonus(List(fo))
    val givenContext = PlayerContext(WestWind, EastWind)

    val thenClosed = List(Knitted(Bamboo, Character, Stone), Dui(dr))
    val thenCombinations =
      List(
        (List(Knitted(Bamboo, Character, Stone)), KnittedStraight),
        (List(Knitted(Bamboo, Character, Stone), Pung(ww), Dui(dr)), AllTypes),
        (List(Pung(ww)), SeatWind),
        (List(Bonus(List(fo))), FlowerTiles)
      )

    test(givenClosed, givenMelded, givenContextualTile, givenBonus, givenContext, thenClosed, thenCombinations, 21)

  }

  test(
    """case Mixed Triple Chow - Fully Concealed Hand, All Chows
      |verif: dubious on OutsideHand
    """.stripMargin) {
    val givenClosed = TileSet(List(b1, b2, b3, b7, b8, b9, c7, c8, c9, s7, s8, s9, dr, dr))
    val givenMelded = Nil
    val givenContextualTile: ContextualTile = ContextualTile(b1, SelfDrawn, NotLastTile)
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(WestWind, EastWind)

    val thenClosed = List(Chow(b1, b2, b3), Chow(b7, b8, b9), Chow(c7, c8, c9), Chow(s7, s8, s9), Dui(dr))
    val thenCombinations =
      List(
        (List(Chow(b7, b8, b9), Chow(c7, c8, c9), Chow(s7, s8, s9)), MixedTripleChow),
        (List(Chow(b1, b2, b3), Chow(b7, b8, b9), Chow(c7, c8, c9), Chow(s7, s8, s9), Dui(dr)), OutsideHand),
        (List(Chow(b1, b2, b3), Chow(b7, b8, b9), Chow(c7, c8, c9), Chow(s7, s8, s9), Dui(dr)), FullyConcealedHand),
        (List(Chow(b1, b2, b3), Chow(b7, b8, b9), Chow(c7, c8, c9), Chow(s7, s8, s9)), AllChows),
        (List(Chow(b1, b2, b3), Chow(b7, b8, b9)), TwoTerminalChows)
      )

    test(givenClosed, givenMelded, givenContextualTile, givenBonus, givenContext, thenClosed, thenCombinations, 19)

  }

  test(
    """case Half Flush - Pung of terminals or honor - Self Drawn - Flowers
      |verif : pymcr m 111d 888d h d33356WeWe w d4 self_draw
      |verif : mahjong friends
    """.stripMargin) {
    val givenClosed = TileSet(List(s2, s2, s2, s5, s6, s4, we, we))
    val givenMelded: List[Figure] = List(Pung(s1), Pung(s8))
    val givenContextualTile: ContextualTile = ContextualTile(s4, SelfDrawn, NotLastTile)
    val givenBonus: Bonus = Bonus(List(fp, fo, sw))
    val givenContext = PlayerContext(WestWind, EastWind)

    val thenClosed = List(Pung(s2), Chow(s4, s5, s6), Dui(we))
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (List(Pung(s1), Pung(s2), Pung(s8), Chow(s4, s5, s6), Dui(we)), HalfFlush),
        (List(Pung(s1)), PungOfTerminalOrHonors),
        (List(Chow(s4, s5, s6)), SelfDrawnComb),
        (List(Bonus(List(fp, fo, sw))), FlowerTiles)
      )

    test(givenClosed, givenMelded, givenContextualTile, givenBonus, givenContext, thenClosed, thenCombinations, 11)

  }

  test(
    """case All Terminal And Honors - 7 pairs - All Types - Fully Concealed Hand
      |verif : http://mahjong.forum2jeux.com/t265-livre-sur-les-regles-officielles-chinoises-v2
    """.stripMargin) {
    val givenClosed = TileSet(List(we, we, dr, dr, dg, dg, b1, b1, b9, b9, c1, c1, s9, s9))
    val givenMelded: List[Figure] = List()
    val givenContextualTile: ContextualTile = ContextualTile(s9, SelfDrawn, NotLastTile)
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(WestWind, EastWind)

    val thenClosed = List(Dui(we), Dui(dr), Dui(dg), Dui(b1), Dui(b9), Dui(c1), Dui(s9))
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (List(Dui(we), Dui(dr), Dui(dg), Dui(b1), Dui(b9), Dui(c1), Dui(s9)), AllTerminalsAndHonors),
        (List(Dui(we), Dui(dr), Dui(dg), Dui(b1), Dui(b9), Dui(c1), Dui(s9)), SevenPairs),
        (List(Dui(we), Dui(dr), Dui(dg), Dui(b1), Dui(b9), Dui(c1), Dui(s9)), AllTypes),
        (List(Dui(we), Dui(dr), Dui(dg), Dui(b1), Dui(b9), Dui(c1), Dui(s9)), FullyConcealedHand)
      )

    test(givenClosed, givenMelded, givenContextualTile, givenBonus, givenContext, thenClosed, thenCombinations, 66)
  }

  test(
    """case Mixed Shifted Chows - All Chows - Short Straight - Single Wait/Closed Wait - Flowere
      |verif : mahjong friends
      |verif : http://mahjong.forum2jeux.com/t442-finir-sur-la-paire#4869
      |tricky part: winning tile b2 can be used to form Chow(b1,b2,b3) or Dui(b2).
      |             So single wait is scored or closed wait is scored, not both.
    """.stripMargin) {
    val givenClosed = TileSet(List(s3, s4, s5, b1, b2, b3, b4, b5, b6, b2, b2))
    val givenMelded: List[Figure] = List(Chow(c5, c6, c7))
    val givenContextualTile: ContextualTile = ContextualTile(b2, Discarded, NotLastTile)
    val givenBonus: Bonus = Bonus(List(fp, fo, ss, sw))
    val givenContext = PlayerContext(WestWind, EastWind)

    val thenClosed = List(Chow(s3, s4, s5), Chow(b1, b2, b3), Chow(b4, b5, b6), Dui(b2))
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (List(Chow(s3, s4, s5), Chow(b4, b5, b6), Chow(c5, c6, c7)), MixedShiftedChow),
        (List(Chow(s3, s4, s5), Chow(b1, b2, b3), Chow(b4, b5, b6), Chow(c5, c6, c7)), AllChows),
        (List(Chow(b1, b2, b3), Chow(b4, b5, b6)), ShortStraight),
        (List(Chow(b1, b2, b3)), ClosedWait),
        (List(Bonus(List(fp, fo, ss, sw))), FlowerTiles)
      )

    test(givenClosed, givenMelded, givenContextualTile, givenBonus, givenContext, thenClosed, thenCombinations, 14)

  }

  test(
    """case Mixed Shifted Chows - All Chows - Single Wait/Edge Wait - Flowers
      |verif : my head
      |verif : http://mahjong.forum2jeux.com/t442-finir-sur-la-paire#4869
      |tricky part: winning tile b3 can be used to form Chow(b1,b2,b3) or Dui(b3).
      |             So single wait is scored or edge wait is scored, not both.
    """.stripMargin) {
    val givenClosed = TileSet(List(s4, s5, s6, b1, b2, b3, b6, b7, b8, b3, b3))
    val givenMelded: List[Figure] = List(Chow(c5, c6, c7))
    val givenContextualTile: ContextualTile = ContextualTile(b3, Discarded, NotLastTile)
    val givenBonus: Bonus = Bonus(List(fp, fo, ss, sw))
    val givenContext = PlayerContext(WestWind, EastWind)

    val thenClosed = List(Chow(s4, s5, s6), Chow(b1, b2, b3), Chow(b6, b7, b8), Dui(b3))
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (List(Chow(s4, s5, s6), Chow(b6, b7, b8), Chow(c5, c6, c7)), MixedShiftedChow),
        (List(Chow(s4, s5, s6), Chow(b1, b2, b3), Chow(b6, b7, b8), Chow(c5, c6, c7)), AllChows),
        (List(Chow(b1, b2, b3)), EdgeWait),
        (List(Bonus(List(fp, fo, ss, sw))), FlowerTiles)
      )

    test(givenClosed, givenMelded, givenContextualTile, givenBonus, givenContext, thenClosed, thenCombinations, 13)

  }

  test(
    """case All Terminals or Honors - Dragon Pung - Seat Wind - One Voided Suit - Pung of Terminals of Honor - Flowers
      |verif : mahjong and friends
      |tricky part: Dragon Pung implies Pung Of Terminal Or Honor but only on a different pung
    """.stripMargin) {
    val givenClosed = TileSet(List(c1, c2, c3, ww, ww, ws, ws, ww))
    val givenMelded: List[Figure] = List(Pung(dr), Pung(b9))
    val givenContextualTile: ContextualTile = ContextualTile(ww, Discarded, NotLastTile)
    val givenBonus: Bonus = Bonus(List(sa))
    val givenContext = PlayerContext(WestWind, EastWind)

    val thenClosed = List(Chow(c1, c2, c3), Pung(ww), Dui(ws))
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (List(Pung(b9), Pung(dr), Chow(c1, c2, c3), Pung(ww), Dui(ws)), OutsideHand),
        (List(Pung(dr)), DragonPung),
        (List(Pung(ww)), SeatWind),
        (List(Pung(b9)), PungOfTerminalOrHonors),
        (List(Pung(b9), Chow(c1, c2, c3)), OneVoidedSuit),
        (List(Bonus(List(sa))), FlowerTiles)
      )

    test(givenClosed, givenMelded, givenContextualTile, givenBonus, givenContext, thenClosed, thenCombinations, 11)

  }

  test(
    """case 2 x Pung of Terminals or Honor
      |verif : My head
      |tricky part: Pung of Terminals or Honor is scored twice (Pung(c9) and Pung(ww))
    """.stripMargin) {
    val givenClosed = TileSet(List(c9, c9, c9, ww, ww, ww, b2, b2))
    val givenMelded: List[Figure] = List(Pung(c8), Chow(b1, b2, b3))
    val givenContextualTile: ContextualTile = ContextualTile(ww, Discarded, NotLastTile)
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List(Pung(c9), Pung(ww), Dui(b2))
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (List(Pung(c9)), PungOfTerminalOrHonors),
        (List(Pung(ww)), PungOfTerminalOrHonors),
        (List(Pung(c8), Pung(c9), Chow(b1, b2, b3), Dui(b2)), OneVoidedSuit)
      )

    test(givenClosed, givenMelded, givenContextualTile, givenBonus, givenContext, thenClosed, thenCombinations, 3)

  }

  test(
    """|verif : Mahjong Friends
      |Note: Mahjong and Friends scored single wait here but I'm not agree because both c2 and c5 are waited
    """.stripMargin) {
    val givenClosed = TileSet(List(s6, s7, s8, c3, c4, c5, c2, c2))
    val givenMelded: List[Figure] = List(Chow(b2, b3, b4), Chow(s4, s5, s6))
    val givenContextualTile: ContextualTile = ContextualTile(c2, SelfDrawn, NotLastTile)
    val givenBonus: Bonus = Bonus(List(fp, fo, fc))
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List(Chow(c3, c4, c5), Chow(s6, s7, s8), Dui(c2))
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (List(Chow(b2, b3, b4), Chow(c3, c4, c5), Chow(s4, s5, s6)), MixedShiftedChow),
        (List(Chow(b2, b3, b4), Chow(c3, c4, c5), Chow(s4, s5, s6), Chow(s6, s7, s8)), AllChows),
        (List(Chow(b2, b3, b4), Chow(c3, c4, c5), Chow(s4, s5, s6), Chow(s6, s7, s8), Dui(c2)), AllSimples),
        //(List(Dui(c2)), SingleWait),
        (List(Dui(c2)), SelfDrawnComb),
        (List(Bonus(List(fp, fo, fc))), FlowerTiles)
      )

    test(givenClosed, givenMelded, givenContextualTile, givenBonus, givenContext, thenClosed, thenCombinations, 14)

  }

  test(
    """No Honors
      |verif : Mahjong Friends
    """.stripMargin) {
    val givenClosed = TileSet(List(s9, s9))
    val givenMelded: List[Figure] = List(Pung(s1), Chow(b1, b2, b3), Chow(b1, b2, b3), Chow(c7, c8, c9))
    val givenContextualTile: ContextualTile = ContextualTile(s9, Discarded, NotLastTile)
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List(Dui(s9))
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (List(Pung(s1), Chow(b1, b2, b3), Chow(b1, b2, b3), Chow(c7, c8, c9), Dui(s9)), MeldedHand),
        (List(Pung(s1), Chow(b1, b2, b3), Chow(b1, b2, b3), Chow(c7, c8, c9), Dui(s9)), OutsideHand),
        (List(Chow(b1, b2, b3), Chow(b1, b2, b3)), PureDoubleChows),
        (List(Pung(s1)), PungOfTerminalOrHonors),
        (List(Pung(s1), Chow(b1, b2, b3), Chow(b1, b2, b3), Chow(c7, c8, c9), Dui(s9)), NoHonors)
      )

    test(givenClosed, givenMelded, givenContextualTile, givenBonus, givenContext, thenClosed, thenCombinations, 13)

  }

  test(
    """use case :
      |verif : Mahjong Friends
      |tricky part:
    """.stripMargin) {
    val givenClosed = TileSet(List(s3, s4, s5, s3, s3))
    val givenMelded: List[Figure] = List(Kong(c5), Kong(dg), Kong(wn))
    val givenContextualTile: ContextualTile = ContextualTile(s3, Discarded, NotLastTile)
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List(Chow(s3), Dui(s3))
    val thenPoints = 36
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (List(Kong(dg), Kong(wn), Kong(c5)), ThreeKongs),
        (List(Kong(dg)), DragonPung),
        (List(Kong(wn)), PungOfTerminalOrHonors),
        (List(Kong(c5), Chow(s3), Dui(s3)), OneVoidedSuit)
      )

    test(givenClosed, givenMelded, givenContextualTile, givenBonus, givenContext, thenClosed, thenCombinations, thenPoints)

  }


  test(
    """use case : Mixed Shifted Pungs - All Pungs - Prevalent Wind
      |verif : Mahjong Friends
      |tricky part:
    """.stripMargin) {
    val givenClosed = TileSet(List(s1, s1, s1, s2, s2))
    val givenMelded: List[Figure] = List(Kong(b3), Pung(c2), Pung(ws))
    val givenContextualTile: ContextualTile = ContextualTile(s1, SelfDrawn, NotLastTile)
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(EastWind, SouthWind)

    val thenClosed = List(Pung(s1), Dui(s2))
    val thenPoints = 19
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (List(Kong(b3), Pung(c2), Pung(s1)), MixedShiftedPung),
        (List(Kong(b3), Pung(c2), Pung(s1), Pung(ws)), AllPungs),
        (List(Pung(ws)), PrevalentWind),
        (List(Pung(s1)), PungOfTerminalOrHonors),
        (List(Kong(b3)), MeldedKong),
        (List(Pung(s1)), SelfDrawnComb)
      )

    test(givenClosed, givenMelded, givenContextualTile, givenBonus, givenContext, thenClosed, thenCombinations, thenPoints)

  }

  test(
    """use case : Two Terminal Chows
      |verif : Mahjong Friends
      |tricky part:
    """.stripMargin) {
    val givenClosed = TileSet(List(s4, s4))
    val givenMelded: List[Figure] = List(Pung(s6), Chow(b2), Chow(s1), Chow(s7))
    val givenContextualTile: ContextualTile = ContextualTile(s4, Discarded, NotLastTile)
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List(Dui(s4))
    val thenPoints = 9
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (List(Pung(s6), Chow(b2), Chow(s1), Chow(s7), Dui(s4)), MeldedHand),
        (List(Chow(s1), Chow(s7)), TwoTerminalChows),
        (List(Pung(s6), Chow(b2), Chow(s1), Chow(s7), Dui(s4)), OneVoidedSuit),
        (List(Pung(s6), Chow(b2), Chow(s1), Chow(s7), Dui(s4)), NoHonors)
      )

    test(givenClosed, givenMelded, givenContextualTile, givenBonus, givenContext, thenClosed, thenCombinations, thenPoints)

  }

  test(
    """use case : Concealed Kong - Double Pung
      |verif : Mahjong Friends
      |tricky part:
    """.stripMargin) {
    val givenClosed = TileSet(List(b6, b6, c8, c8, c8))
    val givenMelded: List[Figure] = List(Kong(s8), Pung(s9))
    val givenConcealedKong = List(Kong(c6))
    val givenContextualTile: ContextualTile = ContextualTile(c8, Discarded, NotLastTile)
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List(Pung(c8), Dui(b6))
    val thenPoints = 27
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (List(Kong(c6), Kong(s8), Pung(c8), Pung(s9), Dui(b6)), UpperFour),
        (List(Kong(c6), Kong(s8), Pung(c8), Pung(s9)), AllPungs),
        (List(Kong(c6), Kong(s8)), TwoMeldedKongs),
        (List(Kong(s8), Pung(c8)), DoublePung),
        (List(Kong(c6)), ConcealedKong),
        (List(Pung(s9)), PungOfTerminalOrHonors)
      )

    test(givenClosed, givenMelded, givenConcealedKong, givenContextualTile, givenBonus, givenContext,
      thenClosed, thenCombinations, thenPoints)

  }

  test(
    """use case : Two concealed pungs
      |verif : Mahjong Friends
      |tricky part:
    """.stripMargin) {
    val givenClosed = TileSet(List(b1, b1, b1, b4, b4, b4, b7, b7, b7, dw, dw))
    val givenMelded: List[Figure] = List(Pung(dg))
    val givenContextualTile: ContextualTile = ContextualTile(b1, Discarded, NotLastTile)
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List(Pung(b1), Pung(b4), Pung(b7), Dui(dw))
    val thenPoints = 17
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (List(Pung(b1), Pung(b4), Pung(b7), Pung(dg)), AllPungs),
        (List(Pung(b1), Pung(b4), Pung(b7), Pung(dg), Dui(dw)), HalfFlush),
        (List(Pung(dg)), DragonPung),
        (List(Pung(b4), Pung(b7)), TwoConcealedPungs),
        (List(Pung(b1)), PungOfTerminalOrHonors)
      )

    test(givenClosed, givenMelded, givenContextualTile, givenBonus, givenContext, thenClosed, thenCombinations, thenPoints)

  }

  test(
    """use case : Tile Hog - Pure Shifted Chow
      |verif : Mahjong Friends
      |tricky part: Non identical principal
    """.stripMargin) {
    val givenClosed = TileSet(List(c8, c8))
    val givenMelded: List[Figure] = List(Chow(c3), Chow(c3), Chow(c4), Chow(c5))
    val givenContextualTile: ContextualTile = ContextualTile(c8, Discarded, NotLastTile)
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List(Dui(c8))
    val thenPoints = 53
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (List(Chow(c3), Chow(c3), Chow(c4), Chow(c5), Dui(c8)), FullFlush),
        (List(Chow(c3), Chow(c4), Chow(c5)), PureShiftedChow),
        (List(Chow(c3), Chow(c3), Chow(c4), Chow(c5), Dui(c8)), MeldedHand),
        (List(Chow(c3), Chow(c3), Chow(c4), Chow(c5)), AllChows),
        (List(Chow(c3), Chow(c3), Chow(c4), Chow(c5)), TileHog),
        (List(Chow(c3), Chow(c3), Chow(c4), Chow(c5), Dui(c8)), AllSimples),
        (List(Chow(c3), Chow(c3)), PureDoubleChows)
      )

    test(givenClosed, givenMelded, givenContextualTile, givenBonus, givenContext, thenClosed, thenCombinations, thenPoints)

  }

  test(
    """use case : Last Tile
      |verif : Mahjong Friends
      |tricky part:
    """.stripMargin) {
    val givenClosed = TileSet(List(c1, c1, c1, s4, s5, s2, s2, s3))
    val givenMelded: List[Figure] = List(Kong(c9), Pung(s3))
    val givenContextualTile: ContextualTile = ContextualTile(s3, Discarded, LastTileOfKind)
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List(Pung(c1), Chow(s3), Dui(s2))
    val thenPoints = 11
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (List(SingleTile(s3)), LastTile),
        (List(Pung(s3), Chow(s3)), TileHog),
        (List(Kong(c9)), PungOfTerminalOrHonors),
        (List(Pung(c1)), PungOfTerminalOrHonors),
        (List(Kong(c9)), MeldedKong),
        (List(Kong(c9), Pung(c1), Pung(s3), Chow(s3), Dui(s2)), OneVoidedSuit),
        (List(Kong(c9), Pung(c1), Pung(s3), Chow(s3), Dui(s2)), NoHonors)
      )

    test(givenClosed, givenMelded, givenContextualTile, givenBonus, givenContext, thenClosed, thenCombinations, thenPoints)

  }


  test(
    """use case : Two Dragons Pungs
      |verif : Mahjong Friends
      |tricky part:
    """.stripMargin) {
    val givenClosed = TileSet(List(s1, s1))
    val givenMelded: List[Figure] = List(Pung(dr), Pung(dg), Chow(c1), Chow(c4))
    val givenContextualTile: ContextualTile = ContextualTile(s1, Discarded, NotLastTile)
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List(Dui(s1))
    val thenPoints = 14
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (List(Pung(dr), Pung(dg), Chow(c1), Chow(c4), Dui(s1)), MeldedHand),
        (List(Pung(dr), Pung(dg)), TwoDragonPungs),
        (List(Chow(c1), Chow(c4)), ShortStraight),
        (List(Chow(c1), Chow(c4), Dui(s1)), OneVoidedSuit)
      )

    test(givenClosed, givenMelded, givenContextualTile, givenBonus, givenContext, thenClosed, thenCombinations, thenPoints)

  }

  test(
    """use case : Two Concealed Pungs
      |verif : Mahjong Friends
      |tricky part: Two concealed pungs and a third with discarded tile
    """.stripMargin) {
    val givenClosed = TileSet(List(s7, s7, s7, c1, c1, c1, dr, dr, dr, ws, ws))
    val givenMelded: List[Figure] = List(Pung(s4))
    val givenContextualTile: ContextualTile = ContextualTile(s7, Discarded, NotLastTile)
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List(Pung(c1), Pung(s7), Pung(dr), Dui(ws))
    val thenPoints = 12
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (List(Pung(c1), Pung(s4), Pung(s7), Pung(dr)), AllPungs),
        (List(Pung(dr)), DragonPung),
        (List(Pung(c1), Pung(dr)), TwoConcealedPungs),
        (List(Pung(c1)), PungOfTerminalOrHonors),
        (List(Pung(c1), Pung(s4), Pung(s7)), OneVoidedSuit)
      )

    test(givenClosed, givenMelded, givenContextualTile, givenBonus, givenContext, thenClosed, thenCombinations, thenPoints)

  }

  test(
    """use case : Two concealed pungs
      |verif : Mahjong Friends
      |tricky part: One of concealed pung is actually a concealed kong
    """.stripMargin) {
    val givenClosed = TileSet(List(s4, s4, s4, c2, c3, c4, s2, s2))
    val givenMelded: List[Figure] = List(Chow(s1))
    val givenConcealedKong = List(Kong(c1))
    val givenContextualTile: ContextualTile = ContextualTile(c4, SelfDrawn, NotLastTile)
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List(Pung(s4), Chow(c2), Dui(s2))
    val thenPoints = 19
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (List(Kong(c1), Pung(s4), Chow(c2), Chow(s1), Dui(s2)), LowerFour),
        (List(Kong(c1), Pung(s4)), TwoConcealedPungs),
        (List(Kong(c1)), ConcealedKong),
        (List(Kong(c1)), PungOfTerminalOrHonors),
        (List(Kong(c1), Pung(s4), Chow(c2), Chow(s1), Dui(s2)), OneVoidedSuit),
        (List(Chow(c2)), SelfDrawnComb)
      )

    test(givenClosed, givenMelded, givenConcealedKong, givenContextualTile, givenBonus, givenContext, thenClosed, thenCombinations, thenPoints)

  }

  test(
    """use case : Concealed Hand - Last Tile
      |verif : Mahjong Friends
      |tricky part:
    """.stripMargin) {
    val givenClosed = TileSet(List(s7, s9, b3, b4, b5, b6, b7, b8, c6, c7, c8, c5, c5, s8))
    val givenMelded: List[Figure] = List()
    val givenContextualTile: ContextualTile = ContextualTile(s8, Discarded, LastTileOfKind)
    val givenConcealedKongs = List()
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List(Chow(b3), Chow(b6), Chow(c6), Chow(s7), Dui(c5))
    val allFigures = (thenClosed ::: givenMelded ::: givenConcealedKongs).sorted(OrdFigure)
    val thenPoints = 11
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (List(SingleTile(s8)), LastTile),
        (allFigures, ConcealedHand),
        (List(Chow(b3), Chow(b6), Chow(c6), Chow(s7)), AllChows),
        (List(Chow(b6), Chow(c6)), MixedDoubleChows),
        (List(Chow(b3), Chow(b6)), ShortStraight),
        (List(Chow(s7)), ClosedWait)
      )

    test(givenClosed, givenMelded, givenContextualTile, givenBonus, givenContext, thenClosed, thenCombinations, thenPoints)

  }

  test(
    """use case : Two concealed kongs
      |verif : My head
      |tricky part:
    """.stripMargin) {
    val givenClosed = TileSet(List(c1, c1))
    val givenMelded: List[Figure] = List(Chow(b2), Chow(s7))
    val givenContextualTile: ContextualTile = ContextualTile(c1, Discarded, NotLastTile)
    val givenConcealedKongs = List(Kong(b2), Kong(c3))
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List(Dui(c1))
    val allFigures = (thenClosed ::: givenMelded ::: givenConcealedKongs).sorted(OrdFigure)
    val thenPoints = 10
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (List(Kong(b2), Kong(c3)), TwoConcealedKongs),
        (allFigures, NoHonors),
        (List(Dui(c1)), SingleWait)
      )

    test(givenClosed, givenMelded, givenConcealedKongs, givenContextualTile, givenBonus, givenContext, thenClosed,
      thenCombinations, thenPoints)

  }

  test(
    """use case : Robbing the kong
      |verif : Mahjong Friends
      |tricky part:
    """.stripMargin) {
    val givenClosed = TileSet(List(b3, b4, c2, c3, c4, c6, c6, b5))
    val givenMelded: List[Figure] = List(Pung(dw), Chow(c6))
    val givenContextualTile: ContextualTile = ContextualTile(b5, KongRobbed, NotLastTile)
    val givenConcealedKongs = List()
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List(Chow(b3), Chow(c2), Dui(c6))
    val allFigures = (thenClosed ::: givenMelded ::: givenConcealedKongs).sorted(OrdFigure)
    val thenPoints = 11
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (List(SingleTile(b5)), RobbingTheKong),
        (List(Pung(dw)), DragonPung),
        (List(Chow(b3), Chow(c2), Chow(c6), Dui(c6)), OneVoidedSuit)
      )

    test(givenClosed, givenMelded, givenContextualTile, givenBonus, givenContext, thenClosed, thenCombinations, thenPoints)

  }

  test(
    """use case : Out with replacement tile
      |verif : My head
      |tricky part:
    """.stripMargin) {
    val givenClosed = TileSet(List(c1, c2, c3, c4, c4))
    val givenMelded: List[Figure] = List(Chow(c6), Chow(s2))
    val givenContextualTile: ContextualTile = ContextualTile(c4, ReplacedTile, NotLastTile)
    val givenConcealedKongs = List(Kong(b2))
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List(Chow(c1), Dui(c4))
    val allFigures = (thenClosed ::: givenMelded ::: givenConcealedKongs).sorted(OrdFigure)
    val thenPoints = 11
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (List(SingleTile(c4)), OutWithRemplacementTile),
        (List(Kong(b2)), ConcealedKong),
        (allFigures, NoHonors)
      )

    test(givenClosed, givenMelded, givenConcealedKongs, givenContextualTile, givenBonus, givenContext, thenClosed,
      thenCombinations,
      thenPoints)

  }

  test(
    """use case : Three Concealed Pungs
      |verif : Mahjong Friends
      |dubious : Mahjong Friends scores 2 x 1 concealed kong (4pts), not 2 concealed kongs (8pts)
      |tricky part:
    """.stripMargin) {
    val givenClosed = TileSet(List(c1, c1, c1, dr, dr))
    val givenMelded: List[Figure] = List(Chow(c5))
    val givenContextualTile: ContextualTile = ContextualTile(dr, Discarded, NotLastTile)
    val givenConcealedKongs = List(Kong(c4), Kong(c9))
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List(Pung(c1), Dui(dr))
    val allFigures = (thenClosed ::: givenMelded ::: givenConcealedKongs).sorted(OrdFigure)
    val thenPoints = 33
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (List(Kong(c4), Kong(c9), Pung(c1)), ThreeConcealedPungs),
        (List(Kong(c4), Kong(c9)), TwoConcealedKongs),
        (List(Kong(c4), Kong(c9), Pung(c1), Chow(c5), Dui(dr)), HalfFlush),
        (List(Kong(c9)), PungOfTerminalOrHonors),
        (List(Pung(c1)), PungOfTerminalOrHonors),
        (List(Dui(dr)), SingleWait)
      )

    test(givenClosed, givenMelded, givenConcealedKongs, givenContextualTile, givenBonus, givenContext, thenClosed, thenCombinations,
      thenPoints)

  }

  test(
    """use case : Last Tile Claim
      |verif : My Head
      |tricky part:
    """.stripMargin) {
    val givenClosed = TileSet(List(s4, s5, s6, c1, c2, c3, b7, b7))
    val givenMelded: List[Figure] = List(Chow(b1), Chow(b7))
    val givenContextualTile: ContextualTile = ContextualTile(c2, Discarded, LastTileClaim)
    val givenConcealedKongs = List()
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List(Chow(c1), Chow(s4), Dui(b7))
    val allFigures = (thenClosed ::: givenMelded ::: givenConcealedKongs).sorted(OrdFigure)
    val thenPoints = 21
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (List(Chow(b7), Chow(c1), Chow(s4)), MixedStraight),
        (List(SingleTile(c2)), LastTileClaimComb),
        (List(Chow(b1), Chow(b7), Chow(c1), Chow(s4)), AllChows),
        (List(Chow(b1), Chow(c1)), MixedDoubleChows),
        (List(Chow(b1), Chow(b7)), TwoTerminalChows),
        (List(Chow(c1)), ClosedWait)
      )

    test(givenClosed, givenMelded, givenContextualTile, givenBonus, givenContext, thenClosed, thenCombinations, thenPoints)

  }

  test(
    """use case : Last Tile Draw
      |verif : Partly Mahjong Friends, My Head for LastTileDraw
      |dubious: I didn't score Self Drawn because of LastTileDraw. It seems common sense to me
      |tricky part:
    """.stripMargin) {
    val givenClosed = TileSet(List(ws, ws))
    val givenMelded: List[Figure] = List(Kong(dr), Pung(ww), Pung(dg), Pung(dw))
    val givenContextualTile: ContextualTile = ContextualTile(ws, SelfDrawn, LastTileDraw)
    val givenConcealedKongs = List()
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List(Dui(ws))
    val allFigures = (thenClosed ::: givenMelded ::: givenConcealedKongs).sorted(OrdFigure)
    val thenPoints = 162
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (List(Kong(dr), Pung(dg), Pung(dw)), BigThreeDragons), //88
        (allFigures, AllHonors), //64
        (List(SingleTile(ws)), LastTileDrawComb), // 8
        (List(Kong(dr)), MeldedKong), // 1
        (List(Dui(ws)), SingleWait) //1
      )

    test(givenClosed, givenMelded, givenConcealedKongs, givenContextualTile, givenBonus, givenContext, thenClosed,
      thenCombinations, thenPoints)

  }
  test(
    """use case : Chicken Hand
      |verif : Mahjong Friends
      |tricky part:
    """.stripMargin) {
    val givenClosed = TileSet(List(s4, s5, s7, s7, s7, b1, b2, b3, we, we, s3))
    val givenMelded: List[Figure] = List(Chow(c7))
    val givenContextualTile: ContextualTile = ContextualTile(s3, Discarded, NotLastTile)
    val givenConcealedKongs = List()
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List(Pung(s7), Chow(b1), Chow(s3), Dui(we))
    val allFigures = (thenClosed ::: givenMelded ::: givenConcealedKongs).sorted(OrdFigure)
    val thenPoints = 8
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (allFigures, ChickenHand)
      )

    test(givenClosed, givenMelded, givenConcealedKongs, givenContextualTile, givenBonus, givenContext, thenClosed,
      thenCombinations, thenPoints)

  }

  test(
    """use case : Chicken Hand with flower
      |verif : Mahjong Friends
      |tricky part:
    """.stripMargin) {
    val givenClosed = TileSet(List(b6, b7, c4, c5, c6, ww, ww, b5))
    val givenMelded: List[Figure] = List(Pung(c7), Chow(s1))
    val givenContextualTile: ContextualTile = ContextualTile(b5, Discarded, NotLastTile)
    val givenConcealedKongs = List()
    val givenBonus: Bonus = Bonus(List(fp, ss))
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List(Chow(b5), Chow(c4), Dui(ww))
    val allFigures = (thenClosed ::: givenMelded ::: givenConcealedKongs).sorted(OrdFigure)
    val thenPoints = 10
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (allFigures, ChickenHand),
        (List(Bonus(List(fp, ss))), FlowerTiles)
      )

    test(givenClosed, givenMelded, givenConcealedKongs, givenContextualTile, givenBonus, givenContext, thenClosed,
      thenCombinations, thenPoints)

  }

  test(
    """use case : Reversible Tiles
      |verif : Mahjong Friends
      |tricky part:
    """.stripMargin) {
    val givenClosed = TileSet(List(s1, s1))
    val givenMelded: List[Figure] = List(Kong(dw), Pung(b5), Pung(b8), Pung(s8))
    val givenContextualTile: ContextualTile = ContextualTile(s1, Discarded, NotLastTile)
    val givenConcealedKongs = List()
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List(Dui(s1))
    val allFigures = (thenClosed ::: givenMelded ::: givenConcealedKongs).sorted(OrdFigure)
    val thenPoints = 25
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (allFigures, ReversibleTiles),
        (List(Kong(dw), Pung(b5), Pung(b8), Pung(s8)), AllPungs),
        (allFigures, MeldedHand),
        (List(Kong(dw)), DragonPung),
        (List(Pung(b8), Pung(s8)), DoublePung),
        (List(Kong(dw)), MeldedKong)
      )

    test(givenClosed, givenMelded, givenConcealedKongs, givenContextualTile, givenBonus, givenContext, thenClosed,
      thenCombinations, thenPoints)

  }

  test(
    """use case : Big Three Winds
      |verif : Mahjong Friends
      |tricky part:
    """.stripMargin) {
    val givenClosed = TileSet(List(b5, b5, b5, c9, c9))
    val givenMelded: List[Figure] = List(Pung(wn), Pung(ww), Pung(ws))
    val givenContextualTile: ContextualTile = ContextualTile(b5, Discarded, NotLastTile)
    val givenConcealedKongs = List()
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(WestWind, NorthWind)

    val thenClosed = List(Pung(b5), Dui(c9))
    val allFigures = (thenClosed ::: givenMelded ::: givenConcealedKongs).sorted(OrdFigure)
    val thenPoints = 23
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (List(Pung(wn), Pung(ww), Pung(ws)), BigThreeWind),
        (List(Pung(b5), Pung(wn), Pung(ww), Pung(ws)), AllPungs),
        (List(Pung(wn)), PrevalentWind),
        (List(Pung(ww)), SeatWind),
        (List(Pung(b5), Dui(c9)), OneVoidedSuit)
      )

    test(givenClosed, givenMelded, givenConcealedKongs, givenContextualTile, givenBonus, givenContext, thenClosed,
      thenCombinations, thenPoints)

  }

  test(
    """use case : Big Three Winds - Pung of Terminal
      |verif : Mahjong Friends
      |tricky part:
    """.stripMargin) {
    val givenClosed = TileSet(List(b5, b5, c9, c9, c9))
    val givenMelded: List[Figure] = List(Pung(wn), Pung(ww), Pung(ws))
    val givenContextualTile: ContextualTile = ContextualTile(b5, Discarded, NotLastTile)
    val givenConcealedKongs = List()
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(WestWind, NorthWind)

    val thenClosed = List(Pung(c9), Dui(b5))
    val allFigures = (thenClosed ::: givenMelded ::: givenConcealedKongs).sorted(OrdFigure)
    val thenPoints = 25
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (List(Pung(wn), Pung(ww), Pung(ws)), BigThreeWind),
        (List(Pung(c9), Pung(wn), Pung(ww), Pung(ws)), AllPungs),
        (List(Pung(wn)), PrevalentWind),
        (List(Pung(ww)), SeatWind),
        (List(Pung(c9)), PungOfTerminalOrHonors),
        (List(Pung(c9), Dui(b5)), OneVoidedSuit),
        (List(Dui(b5)), SingleWait)
      )

    test(givenClosed, givenMelded, givenConcealedKongs, givenContextualTile, givenBonus, givenContext, thenClosed,
      thenCombinations, thenPoints)

  }

  test(
    """use case : Triple Pung
      |verif : My Head
      |tricky part:
    """.stripMargin) {
    val givenClosed = TileSet(List(b2, b3, b4, b4, b4))
    val givenMelded: List[Figure] = List(Pung(b5), Pung(c5), Pung(s5))
    val givenContextualTile: ContextualTile = ContextualTile(b4, Discarded, NotLastTile)
    val givenConcealedKongs = List()
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List(Chow(b2), Dui(b4))
    val allFigures = (thenClosed ::: givenMelded ::: givenConcealedKongs).sorted(OrdFigure)
    val thenPoints = 18
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (List(Pung(b5), Pung(c5), Pung(s5)), TriplePungs),
        (allFigures, AllSimples)
      )

    test(givenClosed, givenMelded, givenConcealedKongs, givenContextualTile, givenBonus, givenContext, thenClosed,
      thenCombinations, thenPoints)

  }

  test(
    """use case : All Five
      |verif : My Head
      |tricky part:
    """.stripMargin) {
    val givenClosed = TileSet(List(s5, s5, c4, c5, c6))
    val givenMelded: List[Figure] = List(Pung(b5), Pung(c5), Chow(s5))
    val givenContextualTile: ContextualTile = ContextualTile(c5, Discarded, NotLastTile)
    val givenConcealedKongs = List()
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List(Chow(c4), Dui(s5))
    val allFigures = (thenClosed ::: givenMelded ::: givenConcealedKongs).sorted(OrdFigure)
    val thenPoints = 21
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (allFigures, AllFive),
        (List(Pung(c5), Chow(c4)), TileHog),
        (List(Pung(b5), Pung(c5)), DoublePung),
        (List(Chow(c4)), ClosedWait)
      )

    test(givenClosed, givenMelded, givenConcealedKongs, givenContextualTile, givenBonus, givenContext, thenClosed,
      thenCombinations, thenPoints)

  }

  test(
    """use case : Three suited terminal chows
      |verif : My head
      |tricky part:
    """.stripMargin) {
    val givenClosed = TileSet(List(s5, s5))
    val givenMelded: List[Figure] = List(Chow(b1), Chow(b7), Chow(c1), Chow(c7))
    val givenContextualTile: ContextualTile = ContextualTile(s5, Discarded, NotLastTile)
    val givenConcealedKongs = List()
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List(Dui(s5))
    val allFigures = (thenClosed ::: givenMelded ::: givenConcealedKongs).sorted(OrdFigure)
    val thenPoints = 22
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (allFigures, ThreeSuitedTerminalChows),
        (allFigures, MeldedHand)
      )

    test(givenClosed, givenMelded, givenConcealedKongs, givenContextualTile, givenBonus, givenContext, thenClosed,
      thenCombinations, thenPoints)

  }

  test(
    """use case : 2 x TileHog
      |verif : My Head
      |tricky part:
    """.stripMargin) {
    val givenClosed = TileSet(List(b2, b2))
    val givenMelded: List[Figure] = List(Pung(b3), Pung(b7), Chow(b3), Chow(b7))
    val givenContextualTile: ContextualTile = ContextualTile(b2, SelfDrawn, NotLastTile)
    val givenConcealedKongs = List()
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List(Dui(b2))
    val allFigures = (thenClosed ::: givenMelded ::: givenConcealedKongs).sorted(OrdFigure)
    val thenPoints = 30
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (allFigures, FullFlush),
        (List(Pung(b3), Chow(b3)), TileHog),
        (List(Pung(b7), Chow(b7)), TileHog),
        (List(Dui(b2)), SingleWait),
        (List(Dui(b2)), SelfDrawnComb)
      )

    test(givenClosed, givenMelded, givenConcealedKongs, givenContextualTile, givenBonus, givenContext, thenClosed,
      thenCombinations, thenPoints)

  }
  test(
    """use case : Pure Straight
      |verif : Mahjong Friends
      |tricky part:
    """.stripMargin) {
    val givenClosed = TileSet(List(b1, b2, b3, b6, b7, b8, b8, b8))
    val givenMelded: List[Figure] = List(Chow(b4), Chow(b7))
    val givenContextualTile: ContextualTile = ContextualTile(b7, SelfDrawn, NotLastTile)
    val givenConcealedKongs = List()
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List(Chow(b1), Chow(b6), Dui(b8))
    val allFigures = (thenClosed ::: givenMelded ::: givenConcealedKongs).sorted(OrdFigure)
    val thenPoints = 45
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (allFigures, FullFlush),
        (List(Chow(b1), Chow(b4), Chow(b7)), PureStraight),
        (List(Chow(b1), Chow(b4), Chow(b6), Chow(b7)), AllChows),
        (List(Chow(b6), Chow(b7), Dui(b8)), TileHog),
        (List(Chow(b6)), SelfDrawnComb)
      )

    test(givenClosed, givenMelded, givenConcealedKongs, givenContextualTile, givenBonus, givenContext, thenClosed,
      thenCombinations, thenPoints)

  }

  test(
    """use case : Quadruple Chow - Lower Tiles
      |verif : My head
      |dubious : Outside hand implied by Lower Tiles
      |tricky part:
    """.stripMargin) {
    val givenClosed = TileSet(List(c1, c1))
    val givenMelded: List[Figure] = List(Chow(b1), Chow(b1), Chow(b1), Chow(b1))
    val givenContextualTile: ContextualTile = ContextualTile(c1, Discarded, NotLastTile)
    val givenConcealedKongs = List()
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List(Dui(c1))
    val allFigures = (thenClosed ::: givenMelded ::: givenConcealedKongs).sorted(OrdFigure)
    val thenPoints = 83
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (List(Chow(b1), Chow(b1), Chow(b1), Chow(b1)), QuadrupleChows),
        (allFigures, LowerTiles),
        (allFigures, MeldedHand),
        (allFigures, OutsideHand),
        (List(Chow(b1), Chow(b1), Chow(b1), Chow(b1), Dui(c1)), OneVoidedSuit)
      )

    test(givenClosed, givenMelded, givenConcealedKongs, givenContextualTile, givenBonus, givenContext, thenClosed,
      thenCombinations, thenPoints)

  }

  test(
    """use case : Pure Triple Chow - Upper Tiles
      |verif : My head
      |dubious : Outside hand implied by Lower Tiles
      |tricky part:
    """.stripMargin) {
    val givenClosed = TileSet(List(c9, c9))
    val givenMelded: List[Figure] = List(Chow(b7), Chow(b7), Chow(b7), Chow(s7))
    val givenContextualTile: ContextualTile = ContextualTile(c9, Discarded, NotLastTile)
    val givenConcealedKongs = List()
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List(Dui(c9))
    val allFigures = (thenClosed ::: givenMelded ::: givenConcealedKongs).sorted(OrdFigure)
    val thenPoints = 61
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (List(Chow(b7), Chow(b7), Chow(b7)), PureTripleChow),
        (allFigures, UpperTiles),
        (allFigures, MeldedHand),
        (allFigures, OutsideHand),
        (List(Chow(b7), Chow(b7), Chow(b7), Chow(s7)), AllChows),
        (List(Chow(b7), Chow(s7)), MixedDoubleChows)
      )

    test(givenClosed, givenMelded, givenConcealedKongs, givenContextualTile, givenBonus, givenContext, thenClosed,
      thenCombinations, thenPoints)

  }

  test(
    """use case : Four concealed pungs - All Even Pungs
      |verif : Mahjong Friends
      |tricky part:
    """.stripMargin) {
    val givenClosed = TileSet(List(b2, b2, b2, b4, b4, b4, c6, c6, c6, s8, s8, s8, b6, b6))
    val givenMelded: List[Figure] = List()
    val givenContextualTile: ContextualTile = ContextualTile(s8, SelfDrawn, NotLastTile)
    val givenConcealedKongs = List()
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List(Pung(b2), Pung(b4), Pung(c6), Pung(s8), Dui(b6))
    val allFigures = (thenClosed ::: givenMelded ::: givenConcealedKongs).sorted(OrdFigure)
    val thenPoints = 92
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (List(Pung(b2), Pung(b4), Pung(c6), Pung(s8)), FourConcealedPungs),
        (allFigures, AllEvenPungs),
        (allFigures, FullyConcealedHand)
      )

    test(givenClosed, givenMelded, givenConcealedKongs, givenContextualTile, givenBonus, givenContext, thenClosed,
      thenCombinations, thenPoints)

  }


  test(
    """use case : Pure Shifted Pungs - Middle Tiles
      |verif : My head
      |tricky part:
    """.stripMargin) {
    val givenClosed = TileSet(List(c5, c5))
    val givenMelded: List[Figure] = List(Pung(b4), Pung(b5), Pung(b6), Pung(s4))
    val givenContextualTile: ContextualTile = ContextualTile(c5, Discarded, NotLastTile)
    val givenConcealedKongs = List()
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List(Dui(c5))
    val allFigures = (thenClosed ::: givenMelded ::: givenConcealedKongs).sorted(OrdFigure)
    val thenPoints = 62
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (List(Pung(b4), Pung(b5), Pung(b6)), PureShiftedPungs),
        (allFigures, MiddleTiles),
        (List(Pung(b4), Pung(b5), Pung(b6), Pung(s4)), AllPungs),
        (allFigures, MeldedHand),
        (List(Pung(b4), Pung(s4)), DoublePung)
      )

    test(givenClosed, givenMelded, givenConcealedKongs, givenContextualTile, givenBonus, givenContext, thenClosed,
      thenCombinations, thenPoints)

  }

  test(
    """use case : Four Shifted Chows by 1
      |verif : My head
      |dubious: Short straight
      |tricky part:
    """.stripMargin) {
    val givenClosed = TileSet(List(dr, dr))
    val givenMelded: List[Figure] = List(Chow(b1), Chow(b2), Chow(b3), Chow(b4))
    val givenContextualTile: ContextualTile = ContextualTile(dr, Discarded, NotLastTile)
    val givenConcealedKongs = List()
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List(Dui(dr))
    val allFigures = (thenClosed ::: givenMelded ::: givenConcealedKongs).sorted(OrdFigure)
    val thenPoints = 45
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (List(Chow(b1), Chow(b2), Chow(b3), Chow(b4)), FourShiftedChows),
        (allFigures, HalfFlush),
        (allFigures, MeldedHand),
        (List(Chow(b1), Chow(b4)), ShortStraight)
      )

    test(givenClosed, givenMelded, givenConcealedKongs, givenContextualTile, givenBonus, givenContext, thenClosed,
      thenCombinations, thenPoints)

  }

  test(
    """use case : Four Shifted Chows by 2
      |verif : My head
      |dubious : Two Terminal Chows
      |tricky part:
    """.stripMargin) {
    val givenClosed = TileSet(List(dr, dr))
    val givenMelded: List[Figure] = List(Chow(b1), Chow(b3), Chow(b5), Chow(b7))
    val givenContextualTile: ContextualTile = ContextualTile(dr, Discarded, NotLastTile)
    val givenConcealedKongs = List()
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List(Dui(dr))
    val allFigures = (thenClosed ::: givenMelded ::: givenConcealedKongs).sorted(OrdFigure)
    val thenPoints = 45
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (List(Chow(b1), Chow(b3), Chow(b5), Chow(b7)), FourShiftedChows),
        (allFigures, HalfFlush),
        (allFigures, MeldedHand),
        (List(Chow(b1), Chow(b7)), TwoTerminalChows)
      )

    test(givenClosed, givenMelded, givenConcealedKongs, givenContextualTile, givenBonus, givenContext, thenClosed,
      thenCombinations, thenPoints)

  }


  test(
    """use case : Four pure shifted pungs
      |verif : My head
      |tricky part:
    """.stripMargin) {
    val givenClosed = TileSet(List(b1, b1))
    val givenMelded: List[Figure] = List(Pung(s6), Pung(s7), Pung(s8), Pung(s9))
    val givenContextualTile: ContextualTile = ContextualTile(b1, Discarded, NotLastTile)
    val givenConcealedKongs = List()
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List(Dui(b1))
    val allFigures = (thenClosed ::: givenMelded ::: givenConcealedKongs).sorted(OrdFigure)
    val thenPoints = 48 + 6 + 1 + 1
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (List(Pung(s6), Pung(s7), Pung(s8), Pung(s9)), FourPureShiftedPungs),
        (allFigures, MeldedHand),
        (List(Pung(s9)), PungOfTerminalOrHonors),
        (allFigures, OneVoidedSuit)
      )

    test(givenClosed, givenMelded, givenConcealedKongs, givenContextualTile, givenBonus, givenContext, thenClosed,
      thenCombinations, thenPoints)

  }

  test(
    """use case : Pure Terminal Chows
      |verif : My head
      |tricky part:
    """.stripMargin) {
    val givenClosed = TileSet(List(b1, b2, b3, b5, b5))
    val givenMelded: List[Figure] = List(Chow(b1), Chow(b7), Chow(b7))
    val givenContextualTile: ContextualTile = ContextualTile(b1, Discarded, NotLastTile)
    val givenConcealedKongs = List()
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List(Chow(b1), Dui(b5))
    val allFigures = (thenClosed ::: givenMelded ::: givenConcealedKongs).sorted(OrdFigure)
    val thenPoints = 64
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (allFigures, PureTerminalChows)
      )

    test(givenClosed, givenMelded, givenConcealedKongs, givenContextualTile, givenBonus, givenContext, thenClosed,
      thenCombinations, thenPoints)

  }

  test(
    """use case: Little three dragons
      |verif : My head
      |tricky part:
    """.stripMargin) {
    val givenClosed = TileSet(List(dr, dr, c4, c5, c6))
    val givenMelded: List[Figure] = List(Pung(dg), Pung(dw), Chow(b2))
    val givenContextualTile: ContextualTile = ContextualTile(c4, Discarded, NotLastTile)
    val givenConcealedKongs = List()
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List(Chow(c4), Dui(dr))
    val allFigures = (thenClosed ::: givenMelded ::: givenConcealedKongs).sorted(OrdFigure)
    val thenPoints = 65
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (List(Pung(dg), Pung(dw), Dui(dr)), LittleThreeDragons),
        (List(Chow(b2), Chow(c4)), OneVoidedSuit)
      )

    test(givenClosed, givenMelded, givenConcealedKongs, givenContextualTile, givenBonus, givenContext, thenClosed,
      thenCombinations, thenPoints)

  }

  test(
    """use case : Little four winds
      |verif : My head
      |tricky part:
    """.stripMargin) {
    val givenClosed = TileSet(List(we, we, c1, c1, c1))
    val givenMelded: List[Figure] = List(Pung(wn), Pung(ww), Pung(ws))
    val givenContextualTile: ContextualTile = ContextualTile(c1, Discarded, NotLastTile)
    val givenConcealedKongs = List()
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List(Pung(c1), Dui(we))
    val allFigures = (thenClosed ::: givenMelded ::: givenConcealedKongs).sorted(OrdFigure)
    val thenPoints = 109
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (List(Pung(wn), Pung(ww), Pung(ws), Dui(we)), LittleFourWinds),
        (allFigures, AllTerminalsAndHonors),
        (List(Pung(c1), Pung(wn), Pung(ww), Pung(ws)), AllPungs),
        (allFigures, HalfFlush),
        (List(Pung(c1)), PungOfTerminalOrHonors)
      )

    test(givenClosed, givenMelded, givenConcealedKongs, givenContextualTile, givenBonus, givenContext, thenClosed,
      thenCombinations, thenPoints)

  }

  test(
    """use case : All Terminals
      |verif : My head
      |tricky part:
    """.stripMargin) {
    val givenClosed = TileSet(List(c1, c1))
    val givenMelded: List[Figure] = List(Pung(b1), Pung(b9), Pung(s1), Pung(s9))
    val givenContextualTile: ContextualTile = ContextualTile(c1, Discarded, NotLastTile)
    val givenConcealedKongs = List()
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List(Dui(c1))
    val allFigures = (thenClosed ::: givenMelded ::: givenConcealedKongs).sorted(OrdFigure)
    val thenPoints = 74
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (allFigures, AllTerminals),
        (allFigures, MeldedHand),
        (List(Pung(b1), Pung(s1)), DoublePung),
        (List(Pung(b9), Pung(s9)), DoublePung)
      )

    test(givenClosed, givenMelded, givenConcealedKongs, givenContextualTile, givenBonus, givenContext, thenClosed,
      thenCombinations, thenPoints)

  }

  test(
    """use case : Seven shifted pairs
      |verif : My head
      |tricky part:
    """.stripMargin) {
    val givenClosed = TileSet(List(b2, b2, b3, b3, b4, b4, b5, b5, b6, b6, b7, b7, b8, b8))
    val givenMelded: List[Figure] = List()
    val givenContextualTile: ContextualTile = ContextualTile(b2, Discarded, NotLastTile)
    val givenConcealedKongs = List()
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List(Dui(b2), Dui(b3), Dui(b4), Dui(b5), Dui(b6), Dui(b7), Dui(b8))
    val allFigures = (thenClosed ::: givenMelded ::: givenConcealedKongs).sorted(OrdFigure)
    val thenPoints = 90
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (allFigures, SevenShiftedPairs),
        (allFigures, AllSimples)
        //        (List(),),
        //        (List(),),
        //        (List(),)
      )

    test(givenClosed, givenMelded, givenConcealedKongs, givenContextualTile, givenBonus, givenContext, thenClosed,
      thenCombinations, thenPoints)

  }

  test(
    """use case : Four Kongs
      |verif : My head
      |dubious on : MeldedKong
      |
      |tricky part:
    """.stripMargin) {
    val givenClosed = TileSet(List(b1, b1))
    val givenMelded: List[Figure] = List(Kong(dr))
    val givenContextualTile: ContextualTile = ContextualTile(b1, Discarded, NotLastTile)
    val givenConcealedKongs = List(Kong(c1), Kong(c2), Kong(c3))
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List(Dui(b1))
    val allFigures = (thenClosed ::: givenMelded ::: givenConcealedKongs).sorted(OrdFigure)
    val thenPoints = 133
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (List(Kong(c1), Kong(c2), Kong(c3), Kong(dr)), FourKongs),
        (List(Kong(c1), Kong(c2), Kong(c3)), PureShiftedPungs),
        (List(Kong(c1), Kong(c2), Kong(c3)), ThreeConcealedPungs),
        (List(Kong(dr)), DragonPung),
        (List(Kong(c1)), PungOfTerminalOrHonors),
        (List(Kong(dr)), MeldedKong),
        (List(Kong(c1), Kong(c2), Kong(c3), Dui(b1)), OneVoidedSuit)
      )

    test(givenClosed, givenMelded, givenConcealedKongs, givenContextualTile, givenBonus, givenContext, thenClosed,
      thenCombinations, thenPoints)

  }

  test(
    """use case : All Green
      |verif : my head
      |tricky part:
    """.stripMargin) {
    val givenClosed = TileSet(List(b6, b6, b8, b8, b8))
    val givenMelded: List[Figure] = List(Pung(b3), Chow(b2))
    val givenContextualTile: ContextualTile = ContextualTile(b8, Discarded, NotLastTile)
    val givenConcealedKongs = List(Kong(dg))
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List(Pung(b8), Dui(b6))
    val allFigures = (thenClosed ::: givenMelded ::: givenConcealedKongs).sorted(OrdFigure)
    val thenPoints = 100
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (allFigures, AllGreen),
        (allFigures, HalfFlush),
        (List(Kong(dg)), DragonPung),
        (List(Pung(b3), Chow(b2)), TileHog),
        (List(Kong(dg)), ConcealedKong)
      )

    test(givenClosed, givenMelded, givenConcealedKongs, givenContextualTile, givenBonus, givenContext, thenClosed,
      thenCombinations, thenPoints)

  }

  test(
    """use case : Nine Gates
      |verif : my head
      |tricky part:
    """.stripMargin) {
    val givenClosed = TileSet(List(b5, b6, b7, b8, b8))
    val givenMelded: List[Figure] = List(Pung(b1), Pung(b9), Chow(b2))
    val givenContextualTile: ContextualTile = ContextualTile(b8, Discarded, NotLastTile)
    val givenConcealedKongs = List()
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List(Chow(b5), Dui(b8))
    val allFigures = (thenClosed ::: givenMelded ::: givenConcealedKongs).sorted(OrdFigure)
    val thenPoints = 89
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (allFigures, NineGates),
        (List(Chow(b2), Chow(b5)), ShortStraight)
      )

    test(givenClosed, givenMelded, givenConcealedKongs, givenContextualTile, givenBonus, givenContext, thenClosed,
      thenCombinations, thenPoints)

  }

  private def test(
                    givenClosed: TileSet,
                    givenMelded: List[Figure],
                    givenContextualTile: ContextualTile,
                    givenBonus: Bonus,
                    givenContext: PlayerContext,
                    thenClosed: List[Figure],
                    thenCombinations: List[(List[Figure], Combination)],
                    thenTotal: Int
                    ) {
    test(givenClosed, givenMelded, noKongs, givenContextualTile, givenBonus, givenContext, thenClosed, thenCombinations, thenTotal)
  }


  private def test(
                    givenClosed: TileSet,
                    givenMelded: List[Figure],
                    givenContextualTile: ContextualTile,
                    givenContext: PlayerContext,
                    thenClosed: List[Figure],
                    thenCombinations: List[(List[Figure], Combination)],
                    thenTotal: Int
                    ) {
    test(givenClosed, givenMelded, noKongs, givenContextualTile, Bonus(Nil), givenContext, thenClosed, thenCombinations, thenTotal)
  }

  private def test(
                    givenClosed: TileSet,
                    givenMelded: List[Figure],
                    givenConcealedKongs: List[Kong],
                    givenContextualTile: ContextualTile,
                    givenBonus: Bonus,
                    givenContext: PlayerContext,
                    thenClosed: List[Figure],
                    thenCombinations: List[(List[Figure], Combination)],
                    thenTotal: Int
                    ) {

    val pts = PlayerTiles(Hand(givenClosed, givenContextualTile),
      givenMelded.sorted(OrdFigure),
      givenConcealedKongs,
      givenBonus)

    val actual = HuFinder(pts, givenContext).find

    val expected = List(
      DetailedPoints(
        HuLe(thenClosed.sorted(OrdFigure),
          givenMelded.sorted(OrdFigure),
          givenContextualTile,
          givenContext,
          givenConcealedKongs,
          givenBonus),
        thenCombinations.map {
          case (figures, combination) => (figures.sorted(OrdFigure), combination)
        })
    )


    assert(actual.size != 0, "No mahjong found : " + pts)
    //assert(actual.size == 1, "Several mahjong found : " + actual)
    assert(actual(0).huLe === expected(0).huLe)
    assert(actual(0) === expected(0))
    assert(actual(0).total === thenTotal)

  }
}
