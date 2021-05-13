package algo
package sudoku

case class Coord(row: Int, col: Int)

object Coord {

  val boxCoords: Seq[Seq[Coord]] = {
    val boxSize = Math.sqrt(Grid.size).toInt
    val coords = (0 until Grid.size).sliding(boxSize, boxSize).toSeq
    for {
      rowCoords <- coords
      colCoords <- coords
    } yield for {
      x <- rowCoords
      y <- colCoords
    } yield Coord(x, y)
  }
}
