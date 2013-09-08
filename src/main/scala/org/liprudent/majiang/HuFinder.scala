package org.liprudent.majiang

import org.liprudent.majiang.tiles.Types._
import org.liprudent.majiang.figures._
import org.liprudent.majiang.computer.FindAllAndReduce
import org.liprudent.majiang.mahjong._
import org.liprudent.majiang.figures.Dui
import org.liprudent.majiang.mahjong.DetailedPoints
import org.liprudent.majiang.tiles.{PlayerTiles, PlayerContext}

case class HuFinder(ptiles: PlayerTiles, context: PlayerContext) {

  /**
   *
   * @return A list ordered by desc total. Each element is a detailed solution for given <code>ptiles</code>.
   */
  lazy val find: List[DetailedPoints] = {
    if (!quickValid) Nil
    else {
      val computer = FindAllAndReduce(ptiles.concealed.tileSet)

      computer.allFiguresCombinations.foldLeft(List[DetailedPoints]()) {
        (res, concealed) =>
          if (HuFinder.isWellFormedMahjong(concealed, ptiles.melded, ptiles.concealedKongs)) {
            val lastTile = ptiles.concealed.lastTileContext.tile
            val waitingTiles = (lastTile ::
              UniqueWait.waitingTiles(ptiles.concealed.withoutLastTile.tileSet, ptiles.melded,
                ptiles.concealedKongs, List(lastTile))
              ).sorted

            val hule = HuLe(concealed, ptiles.melded, ptiles.concealed.lastTileContext, context, waitingTiles,
              ptiles.concealedKongs, ptiles.bonus)

            HulePointsComputer(hule) :: res
          } else {
            res
          }
      }.sortWith((detail1, detail2) => detail1.total >= detail2.total)
    }
  }

  lazy val quickValid = {
    ptiles.numberOfTiles == 14 + ptiles.numberOfKongs
  }

}

object HuFinder {
  def isWellFormedMahjong(closed: Figures, disclosed: Figures, concealedKongs: List[Kong]): Boolean = {
    val all = closed ::: disclosed ::: concealedKongs
    //classical mahjong hand
    classicalMahjondHand(all) ||
      //knitted staight hand
      knittedStraightHand(all) ||
      //partial or complete knitted + unique dragons
      someKnittedSomeDragons(closed) ||
      thirteenOrphans(closed) ||
      sevenPairs(all)
  }

  def sevenPairs(all: List[Figure]) = {
    all.size == 7 && all.forall(_.isInstanceOf[Dui])
  }

  def thirteenOrphans(closed: List[Figure]): Boolean = {
    closed match {
      case ThirteenOrphans(_) :: Nil => true
      case _ => false
    }
  }

  def someKnittedSomeDragons(closed: List[figures.Figure]): Boolean = {
    closed.size == 1 &&
      closed(0).isInstanceOf[SomeKnittedWithSomeDragons]
  }

  def knittedStraightHand(all: List[figures.Figure]): Boolean = {
    all.size == 3 && all.exists(_.isInstanceOf[Knitted]) &&
      all.filter(_.properties.size == 3).size == 1 &&
      all.filter(_.properties.size == 2).size == 1
  }


  def classicalMahjondHand(all: List[figures.Figure]): Boolean = {
    all.size == 5 &&
      all.filter(_ match {
        case f: Kong => true
        case f: Pung => true
        case f: Chow => true
        case _ => false
      }).size == 4 &&
      all.filter(_ match {
        case f: Dui => true
        case _ => false
      }).size == 1
  }
}