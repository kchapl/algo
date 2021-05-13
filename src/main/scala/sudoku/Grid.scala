package algo
package sudoku

import sudoku.Coord.boxCoords
import sudoku.Pred.{allUnique, containsADeducedCell, isDeduced}

import scala.annotation.tailrec

case class Grid(rows: Seq[Seq[Cell]]) {

  def cellAt(coord: Coord): Cell = rows(coord.row)(coord.col)

  lazy val coordsOfLastCellFilled: Option[Coord] = {
    val row = rows.lastIndexWhere(containsADeducedCell)
    if (row == -1) None
    else Some(Coord(row, rows(row).lastIndexWhere(isDeduced)))
  }

  lazy val nextCellFilled: Grid = {
    val coordsOfNextCellToFill: Option[Coord] = {
      val row = rows.indexWhere(_.contains(Empty))
      if (row == -1) None
      else Some(Coord(row, rows(row).indexWhere(_ == Empty)))
    }
    coordsOfNextCellToFill
      .map(updated(Deduced(1)))
      .getOrElse(this)
  }

  def updated(value: Cell)(c: Coord): Grid =
    Grid(rows.updated(c.row, rows(c.row).updated(c.col, value)))

  def colValues(colIndex: Int): Seq[Cell] =
    rows.map(_(colIndex))

  lazy val isValid: Boolean = {
    val rowsValid: Boolean = rows forall { row =>
      allUnique(row.filterNot(_ == Empty))
    }
    val colsValid: Boolean =
      rows.indices forall { colIndex =>
        val values = colValues(colIndex).filterNot(_ == Empty)
        allUnique(values)
      }
    val boxesValid: Boolean = boxCoords forall { box =>
      val values = box.map(cellAt).filterNot(_ == Empty)
      allUnique(values)
    }
    rowsValid && colsValid && boxesValid
  }

  lazy val isComplete: Boolean =
    (0 until Grid.size).forall(i => !rows(i).contains(Empty)) && isValid

  lazy val incremented: Grid = {
    @tailrec
    def go(acc: Grid): Grid =
      if (acc.isComplete) acc
      else
        acc.coordsOfLastCellFilled match {
          case None => acc
          case Some(last) =>
            if (acc.cellAt(last).value == Grid.size)
              go(acc.updated(Empty)(last))
            else {
              val updated = acc.updated(
                Deduced(acc.cellAt(last).value + 1)
              )(last)
              if (updated.isValid) updated
              else go(updated)
            }
        }
    go(this)
  }

  lazy val solved: Grid = {
    @tailrec
    def go(acc: Grid, numSteps: Int): Grid =
      if (acc.isComplete || numSteps == 20000) {
        println(numSteps)
        acc
      } else {
        val n = acc.nextCellFilled
        if (n.isValid) go(n, numSteps + 1)
        else
          go(n.incremented, numSteps + 1)
      }
    go(this, 0)
  }

  lazy val show: String =
    rows.map { row =>
      val s = row.map {
        case Empty => "- "
        case cell  => s"${cell.value} "
      }.mkString
      s"$s\n"
    }.mkString
}

object Grid {
  val size = 9
}
