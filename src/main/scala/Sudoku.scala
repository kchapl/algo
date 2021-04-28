package algo

import scala.annotation.tailrec

object Sudoku extends App {

  sealed trait Cell { def value: Int }
  case class Given(value: Int) extends Cell
  case class Deduced(value: Int) extends Cell
  case object Empty extends Cell { val value = 0 }

  case class Coord(row: Int, col: Int)

  object Coord {

    def coordsOfNextCellToFill(g: Grid): Option[Coord] = {
      val row = g.rows.indexWhere(row => row.contains(Empty))
      if (row == -1) None
      else {
        val h = g.rows(row).indexWhere(_ == Empty)
        Some(Coord(row, h))
      }
    }

    def coordsOfLastCellFilled(g: Grid): Option[Coord] = {
      val row =
        g.rows.lastIndexWhere(row => row.exists(c => c.isInstanceOf[Deduced]))
      if (row == -1) None
      else {
        val h = g.rows(row).lastIndexWhere(c => c.isInstanceOf[Deduced])
        Some(Coord(row, h))
      }
    }
  }

  case class Grid(rows: Seq[Array[Cell]])

  def nextCellFilled(g: Grid): Grid = {
    val c = Coord.coordsOfNextCellToFill(g)
    c.foreach(d => g.rows(d.row)(d.col) = Deduced(1))
    g
  }

  def incremented(g: Grid): Grid = {

    @tailrec
    def go(acc: Grid): Grid =
      if (isComplete(acc)) acc
      else {
        val lastFilled = Coord.coordsOfLastCellFilled(acc)
        lastFilled match {
          case None => acc
          case Some(last) =>
            val cellValue = acc.rows(last.row)(last.col)
            if (cellValue.value == 9) {
              acc.rows(last.row)(last.col) = Empty
              val n = Grid(acc.rows)
              go(n)
            } else {
              acc.rows(last.row)(last.col) = Deduced(
                acc.rows(last.row)(last.col).value + 1
              )
              if (isValid(acc)) acc
              else go(acc)
            }
        }
      }

    go(g)
  }

  def rowsValid(g: Grid): Boolean = g.rows.forall { row =>
    val knownValues = row.filterNot(_ == Empty)
    knownValues.map(_.value).distinct.length == knownValues.length
  }

  def colValues(g: Grid, colIndex: Int): Seq[Cell] = {
    val x = g.rows.map { row =>
      row(colIndex)
    }
    x
  }

  def colsValid(g: Grid): Boolean =
    g.rows.indices forall { colIndex =>
      val values = colValues(g, colIndex).filterNot(_ == Empty)
      values.map(_.value).distinct.length == values.length
    }

  def boxCoords(g: Grid): Seq[Seq[Coord]] =
    Seq(
      Seq(
        Coord(0, 0),
        Coord(0, 1),
        Coord(0, 2),
        Coord(1, 0),
        Coord(1, 1),
        Coord(1, 2),
        Coord(2, 0),
        Coord(2, 1),
        Coord(2, 2)
      ),
      Seq(
        Coord(0, 3),
        Coord(0, 4),
        Coord(0, 5),
        Coord(1, 3),
        Coord(1, 4),
        Coord(1, 5),
        Coord(2, 3),
        Coord(2, 4),
        Coord(2, 5)
      ),
      Seq(
        Coord(0, 6),
        Coord(0, 7),
        Coord(0, 8),
        Coord(1, 6),
        Coord(1, 7),
        Coord(1, 8),
        Coord(2, 6),
        Coord(2, 7),
        Coord(2, 8)
      ),
      Seq(
        Coord(3, 0),
        Coord(3, 1),
        Coord(3, 2),
        Coord(4, 0),
        Coord(4, 1),
        Coord(4, 2),
        Coord(5, 0),
        Coord(5, 1),
        Coord(5, 2)
      ),
      Seq(
        Coord(3, 3),
        Coord(3, 4),
        Coord(3, 5),
        Coord(4, 3),
        Coord(4, 4),
        Coord(4, 5),
        Coord(5, 3),
        Coord(5, 4),
        Coord(5, 5)
      ),
      Seq(
        Coord(3, 6),
        Coord(3, 7),
        Coord(3, 8),
        Coord(4, 6),
        Coord(4, 7),
        Coord(4, 8),
        Coord(5, 6),
        Coord(5, 7),
        Coord(5, 8)
      ),
      Seq(
        Coord(6, 0),
        Coord(6, 1),
        Coord(6, 2),
        Coord(7, 0),
        Coord(7, 1),
        Coord(7, 2),
        Coord(8, 0),
        Coord(8, 1),
        Coord(8, 2)
      ),
      Seq(
        Coord(6, 3),
        Coord(6, 4),
        Coord(6, 5),
        Coord(7, 3),
        Coord(7, 4),
        Coord(7, 5),
        Coord(8, 3),
        Coord(8, 4),
        Coord(8, 5)
      ),
      Seq(
        Coord(6, 6),
        Coord(6, 7),
        Coord(6, 8),
        Coord(7, 6),
        Coord(7, 7),
        Coord(7, 8),
        Coord(8, 6),
        Coord(8, 7),
        Coord(8, 8)
      )
    )

  def boxesValid(g: Grid): Boolean = boxCoords(g).forall { box =>
    val values = box
      .map { b =>
        g.rows(b.row)(b.col)
      }
      .filterNot(_ == Empty)
    values.map(_.value).distinct.length == values.length
  }

  def isValid(g: Grid): Boolean =
    rowsValid(g) && colsValid(g) && boxesValid(g)

  def isComplete(g: Grid): Boolean =
    (0 to 8).forall(i => !g.rows(i).contains(Empty)) && isValid(g)

  def show(g: Grid): String =
    g.rows.map { row =>
      val x = row.map {
        case Empty => "- "
        case cell  => s"${cell.value} "
      }.mkString
      s"$x\n"
    }.mkString

  def solved(g: Grid): Grid = {
    @tailrec
    def go(acc: Grid, numSteps: Int): Grid =
      if (isComplete(acc) || numSteps == 20000) {
        println(numSteps)
        acc
      } else {
        val n = nextCellFilled(acc)
        if (isValid(n)) go(n, numSteps + 1)
        else {
          val i = incremented(n)
          go(i, numSteps + 1)
        }
      }
    go(g, 0)
  }
}
