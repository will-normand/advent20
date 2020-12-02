package advent20.utils

import scala.io.Source

class InputFileReader(filename: String) {
  def getLines: List[String] = {
    val source = Source.fromResource(filename)
    val lines = source.getLines().toList
    source.close()
    lines
  }
}
