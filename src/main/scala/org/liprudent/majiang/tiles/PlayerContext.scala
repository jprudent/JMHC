package org.liprudent.majiang.tiles

/**
 * Context of the game
 * @param seatWind Player's wind
 * @param prevalentWind Game's wind
 */
case class PlayerContext(seatWind: WindFamily, prevalentWind: WindFamily)
