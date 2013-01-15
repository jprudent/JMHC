package org.liprudent.majiang.computer

import org.liprudent.majiang.tiles.Types._


trait FiguresComputer {
  /**
   * Find all possible combinations of tiles
   *
   * @return Set of all figures combinations
   */
  def allFiguresCombinations: Set[Figures]
}
