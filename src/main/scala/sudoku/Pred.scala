package algo
package sudoku

object Pred {

  def containsADeducedCell(cells: Seq[Cell]): Boolean =
    cells.collectFirst { case _: Deduced => () }.isDefined

  def isDeduced(cell: Cell): Boolean =
    cell match {
      case _: Deduced => true
      case _          => false
    }

  def allUnique(cells: Seq[Cell]): Boolean =
    cells.map(_.value).distinct.length == cells.length
}
