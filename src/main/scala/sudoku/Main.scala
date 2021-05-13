package algo
package sudoku

import scala.io.StdIn.readLine

object Main extends App {

  private val digit = """(\d)""".r

  def coordinateValue(c: Coord): Cell = {
    print(s"Row ${c.row + 1}, column ${c.col + 1}: ")
    readLine() match {
      case digit(d) => Given(d.toInt)
      case _        => Empty
    }
  }

  val rows = for {
    row <- 0 until Grid.size
  } yield for {
    col <- 0 until Grid.size
  } yield coordinateValue(Coord(row, col))

  val grid = Grid(rows)
  println()
  println(grid.show)
  println(grid.solved.show)
}
