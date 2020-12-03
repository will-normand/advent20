package advent20

import advent20.utils.InputFileReader

object Day03 extends App {
  val initialMap = new InputFileReader("input03").getLines
  val map = new TobogganMap(initialMap)

  println("Trees hit: " + map.treesHit(1, 3))

  val routeOptions = List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
  val routeProduct: Int = routeOptions.map(r => map.treesHit(r._2, r._1)).product
  println("Product of trees hit: " + routeProduct)

  class TobogganMap(data: List[String]) {
    def lazyRow(first: String): LazyList[Char] = first.to(LazyList) #::: lazyRow(first)

    val fullMap: List[LazyList[Char]] = data map lazyRow

    def treesHit(down: Int, right: Int): Int = {
      val hits = for {
        x <- fullMap.zipWithIndex
        y = x._2 * right / down
        ground = x._1(y)
        if x._2 % down == 0
      } yield ground
      hits.count(_ == '#')
    }
  }

}
