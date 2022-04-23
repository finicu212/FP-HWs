import scala.annotation.tailrec

object Main {
  type Line = List[Player]
  type Board = List[Line]

  def profileID:Int = 754830

  // creates a board from a string
  def makeBoard(s: String): Board = {
    def toPos(c: Char): Player =
      c match {
        case 'X' => One
        case '0' => Two
        case _ => Empty
      }

    def aux(text: List[Char]): Board = text match {
      case '\r' :: '\n' :: xs => Nil :: aux(xs) // I spent about 6 hours debugging until I figured out it had CRLF breaks...
      case '\r' :: xs => Nil :: aux(xs)
      case '\n' :: xs => Nil :: aux(xs)
      case x :: xs => aux(xs) match {
        case y :: ys => (toPos(x) :: y) :: ys
        case Nil => List(List[Player](toPos(x)))
      }
      case Nil => Nil
    }

    aux(s.toList)
  }

  // checks if the position (x,y) board b is free
  @tailrec
  def isFree(x: Int, y: Int, b: Board): Boolean = {
    @tailrec
    def isFreeLine(x: Int, l: Line): Boolean = l match {
      case head :: tail =>
        if (x > 0) isFreeLine(x - 1, tail)
        else head.equals(Empty)
    }

    b match {
      // navigate vertically
      case firstLine :: otherLines =>
        if (y > 0) isFree(x, y - 1, otherLines)
        // navigate horizontally
        else isFreeLine(x, firstLine)
    }
  }

  // returns the "other" player from a position, if it exists
  def complement(p: Player): Player = p match {
    case One => Two
    case Two => One
    case _ => p
  }

  def show(b: Board): String = {
    def showLine(l: Line): String = l match {
      case One :: xs => 'X' + showLine(xs)
      case Two :: xs => '0' + showLine(xs)
      case _ :: xs => '.' + showLine(xs)
      case Nil => ""
    }
    b match {
      case x :: xs =>
        if (xs.nonEmpty) showLine(x) + "\n" + show(xs)
        else showLine(x)
      case _ => ""
    }
  }

  // Returns a list of columns from a board
  def getColumns(b:Board): Board = b

  //returns the first diagonal as a line
  /**
   * Append top left char to getFstDiag(minor) until minor is empty
   *
   * My algorithm of getting the "minor" of the board:
   * 1. Get rid of first line
   * 2. Transpose what's left
   * 3. Get rid of first line of that
   * 4. Transpose what's left
   * 5. Result should be the minor
   * */
  def getFstDiag(b:Board): Line = ???

  //returns the second diagonal as a line
  def getSndDiag(b:Board): Line = ???

  // retrieves all the diagonals above the first line
  def getAboveFstDiag(b: Board): List[Line] = ???

  def getBelowFstDiag(b: Board): List[Line] = ???

  def getAboveSndDiag(b: Board): List[Line] = ???

  def getBelowSndDiag(b: Board): List[Line] = ???

  //write a function which checks if a given player is a winner
  //hints: patterns and exists
  def winner(p: Player)(b: Board): Boolean = {
    ???
  }

  /*
   * Write a function which updates a position (with a player) at given indices from the board.
   * Your function need not check if the position is empty.
   * Partial stub - you can remove it if you want to implement it another way
   */

  def update(p: Player)(ln: Int, col: Int, b: Board) : Board = ???


  /*
   * generates one possible next move for player p. Hint - use "isFree" and "update"
   *
   * */
  def next(p: Player)(b: Board): List[Board] = ???


  // for testing purposes only.
  def main(args: Array[String]) = {

  }
}

