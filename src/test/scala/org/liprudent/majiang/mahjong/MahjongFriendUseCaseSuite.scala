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
    val givenContextualTile: ContextualTile = ContextualTile(s3, Discarded,false)
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List()
    val thenPoints =
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (List(),),
        (List(),),
        (List(),),
        (List(),),
        (List(),)
      )

    test(givenClosed, givenMelded, givenContextualTile, givenBonus, givenContext, thenClosed, thenCombinations, thenPoints)

  }
 * }}}
 */
@RunWith(classOf[JUnitRunner])
class MahjongFriendUseCaseSuite extends FunSuite {

  test(
    """"case MeldedHand AllChows MixedDoubleChows
      |verif: dubious on One Voided Suit
    """.stripMargin) {
    val givenClosed = TileSet(List(b3, b3))
    val givenMelded = List(Chow(b1, b2, b3), Chow(b5, b6, b7), Chow(c5, c6, c7), Chow(c7, c8, c9))
    val givenContextualTile = ContextualTile(b3, Discarded, false)
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
    val givenContextualTile = ContextualTile(s9, Discarded, false)
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
    val givenContextualTile: ContextualTile = ContextualTile(s8, Discarded, false)
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
    val givenContextualTile: ContextualTile = ContextualTile(dg, Discarded, false)
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
    val givenContextualTile: ContextualTile = ContextualTile(b1, SelfDrawn, false)
    val givenBonus: Bonus = Bonus(List(fo))
    val givenContext = PlayerContext(WestWind, EastWind)

    val thenClosed = List(Knitted(Bamboo, Character, Stone), Pung(c8), Dui(dr))
    val thenCombinations =
      List(
        (List(Knitted(Bamboo, Character, Stone)), KnittedStraight),
        (List(Knitted(Bamboo, Character, Stone), Pung(c8), Dui(dr)), FullyConcealedHand),
        (List(Bonus(List(fo))), FlowerTiles)
      )

    test(givenClosed, givenMelded, givenContextualTile, givenBonus, givenContext, thenClosed, thenCombinations, 17)

  }

  test("case Knitted Straight - All Types") {

    val givenClosed = TileSet(List(b1, b4, b7, c2, c5, c8, s3, s6, s9, dr, dr))
    val givenMelded = List(Pung(ww))
    val givenContextualTile: ContextualTile = ContextualTile(b1, Discarded, false)
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
    val givenContextualTile: ContextualTile = ContextualTile(b1, SelfDrawn, false)
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
    val givenContextualTile: ContextualTile = ContextualTile(s4, SelfDrawn, false)
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
    val givenContextualTile: ContextualTile = ContextualTile(s9, SelfDrawn, false)
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
    val givenContextualTile: ContextualTile = ContextualTile(b2, Discarded, false)
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
    val givenContextualTile: ContextualTile = ContextualTile(b3, Discarded, false)
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
    val givenContextualTile: ContextualTile = ContextualTile(ww, Discarded, false)
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
    val givenContextualTile: ContextualTile = ContextualTile(ww, Discarded, false)
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
    val givenContextualTile: ContextualTile = ContextualTile(c2, SelfDrawn, false)
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
    val givenContextualTile: ContextualTile = ContextualTile(s9, Discarded, false)
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
    val givenContextualTile: ContextualTile = ContextualTile(s3, Discarded, false)
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
    val givenContextualTile: ContextualTile = ContextualTile(s1, SelfDrawn, false)
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
    val givenContextualTile: ContextualTile = ContextualTile(s4, Discarded, false)
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
    val givenContextualTile: ContextualTile = ContextualTile(c8, Discarded, false)
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
    val givenContextualTile: ContextualTile = ContextualTile(b1, Discarded, false)
    val givenBonus: Bonus = Bonus(Nil)
    val givenContext = PlayerContext(EastWind, EastWind)

    val thenClosed = List(Pung(b1), Pung(b4), Pung(b7), Dui(dw))
    val thenPoints = 17
    val thenCombinations: List[(List[Figure], Combination)] =
      List(
        (List(Pung(b1), Pung(b4), Pung(b7), Pung(dg), Dui(dw)), HalfFlush),
        (List(Pung(b1), Pung(b4), Pung(b7), Pung(dg)), AllPungs),
        (List(Pung(dg)), DragonPung),
        (List(Pung(b4), Pung(b7)), TwoConcealedPungs),
        (List(Pung(b1)), PungOfTerminalOrHonors)
      )

    test(givenClosed, givenMelded, givenContextualTile, givenBonus, givenContext, thenClosed, thenCombinations, thenPoints)

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

    assert(actual.size == 1, "Not a valid HuLe : " + pts)
    assert(actual(0).huLe === expected(0).huLe)
    assert(actual === expected)
    assert(actual(0).total === thenTotal)

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

}
