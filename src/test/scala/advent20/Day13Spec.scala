package advent20

import advent20.Day13.BusScheduler
import org.scalatest.flatspec.AnyFlatSpec

class Day13Spec extends AnyFlatSpec {
  "BusScheduler" should " work for 17,x,13,19 " in {
    assertResult(3417) {
      val scheduler = BusScheduler("17,x,13,19")
      println(scheduler.buses)
      scheduler.busWaveTimestamp
    }
  }
}
