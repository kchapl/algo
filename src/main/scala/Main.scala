package algo

import Sudoku._

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

  val y = for {
    row <- 0 to 8
  } yield for {
    col <- 0 to 8
  } yield coordinateValue(Coord(row, col))

  val w = Grid(y)
  println()
  println(show(w))
  println(show(solved(w)))
}
