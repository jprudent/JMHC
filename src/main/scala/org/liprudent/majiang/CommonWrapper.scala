package org.liprudent.majiang

import org.liprudent.majiang.mahjong.DetailedPoints


object CommonWrapper {

  def toMap(detailedPoints: DetailedPoints) = {
    Map(
      ("total", detailedPoints.total),
      ("combinations", detailedPoints.detailedPoints.map {
        case (figures, combination) =>
          Map(
            ("figures", figures.map(_.toTiles.mkString("-"))),
            ("combination", Map(("name", combination.name),
              ("points", combination.points))))
      }))
  }
}