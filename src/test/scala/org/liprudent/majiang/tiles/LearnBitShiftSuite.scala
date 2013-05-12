package org.liprudent.majiang.tiles

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.liprudent.majiang.tiles.Tile._


@RunWith(classOf[JUnitRunner])
class LearnBitShiftSuite extends FunSuite {

  test("left shift") {

    val i  = Integer.parseInt("01000000000000000000000000000000",2)
    println(Integer.toString(i,2))
    val j = i << 1
    println(Integer.toString(j,2))
    val k = i << 2
    println(Integer.toString(k,2))


  }

  test("concrete") {
    val bamboos = Integer.parseInt("432101234", 8)

    val z = bamboos & Integer.parseInt("000700000",8)
    assert(z === Integer.parseInt("100000",8))

    val occurence = z >>> (5*3)
    assert(occurence === 1)
  }


}