package advent20

import java.time.LocalDateTime

import advent20.utils.InputFileReader

import scala.annotation.tailrec

object Day13 extends App {

  val lines = new InputFileReader("input13").getLines

  val timestamp = lines.head.toInt
  val buses = lines.tail.head.split(',').flatMap(_.toIntOption).toList
  val busCycles = buses.zip(buses.map((x: Int) => math.ceil(timestamp.toDouble / x).toInt))
  val waits = busCycles.zip(busCycles.map(x => (x._1 * x._2) - timestamp))
  val shortestWait = waits.minBy(_._2)
  val answer = shortestWait._1._1 * shortestWait._2
  println("Answer (part 1) " + answer)

  val answer2 = BusScheduler(lines.tail.head).busWaveTimestamp
  println("Answer (part 2) " + answer2)

  case class BusScheduler(busTimes: String) {
    val busesStrings: List[Option[Int]] = busTimes.split(',').map(_.toIntOption).toList
    val buses: Map[Int, Int] = busesStrings.zipWithIndex.filter(_._1.isDefined).map(bus => (bus._2, bus._1.get)).toMap
    val slowestBus: (Int, Int) = buses.maxBy(_._2)

    def busWaveTimestamp: BigInt = {
      @tailrec
      def innerLoop(index: Int, bus: Int, count: BigInt, step: BigInt): (BigInt, BigInt) = {
        if (((count + index) % bus) == 0)
          (count, step * bus)
        else
          innerLoop(index, bus, count + step, step)
      }

      var firstBusTime = BigInt(0)
      var step = BigInt(1)
      buses.foreach { case (index, bus) =>
        val result = innerLoop(index, bus, firstBusTime, step)
        firstBusTime = result._1
        step = result._2
      }
      firstBusTime
    }

    def busWaveTimestampBruteForce: BigInt = {

      def numbers(start: BigInt): Iterator[BigInt] = new Iterator[BigInt] {
        private var i = start

        def hasNext = true

        def next(): BigInt = {
          i += 1;
          i
        }
      }

      val start = BigInt("100000000000000") / slowestBus._2
      numbers(start).find(busWaveFrom) match {
        case Some(wave) => wave * slowestBus._2 - slowestBus._1
      }
    }

    def busWaveFrom(i: BigInt): Boolean = {
      if (i % 1_000_000 == 0) {
        println(LocalDateTime.now() + " Got to " + i * slowestBus._2)
      }

      val slowestBusIndex = slowestBus._1
      val slowestBusTime = slowestBus._2 * i

      buses forall { case (index, bus) => ((slowestBusTime + (index - slowestBusIndex)) % bus) == 0 }
    }
  }
}
