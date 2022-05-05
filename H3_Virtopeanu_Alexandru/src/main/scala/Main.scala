import scala.annotation.tailrec

object Main {
  type Line = List[Player]
  type Board = List[Line]

  def profileID:Int = 754830

  def newline: String = "\r\n"

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
      case _ => false
    }

    b match {
      // navigate vertically
      case firstLine :: otherLines =>
        if (y > 0) isFree(x, y - 1, otherLines)
        // navigate horizontally
        else isFreeLine(x, firstLine)
      case _ => false
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
      case x :: Nil => showLine(x)
      case x :: xs  => showLine(x) + newline + show(xs)
      case _ => ""
    }
  }

  // Returns a list of columns from a board
  def getColumns(b:Board): Board = {
    def aux(b: Board, boardNextIter: Board = Nil, accCurrentLine: Line = Nil): Board = b match {
      case line :: lines => line match {
        case x :: xs => aux(lines, boardNextIter :+ xs, accCurrentLine :+ x)
        case Nil =>
          // usual square board
          if (boardNextIter == Nil) Nil
          // additional chance to stay in aux for non-square boards (for above-diags funcs)
          else accCurrentLine :: aux(boardNextIter)
      }
      case Nil => accCurrentLine :: aux(boardNextIter)
    }
    aux(b)
  }

  def mirrorXAxis(b: Board): Board = for (line <- b) yield line.reverse
  def mirrorYAxis(b: Board): Board = b.reverse
  def mirrorXYAxis(b: Board): Board = mirrorYAxis(mirrorXAxis(b))

  /** algo of getting the "minor" of the board:
    * 1. Get rid of first line
    * 2. Transpose what's left
    * 3. Get rid of first line of that
    * 4. Transpose what's left
    * 5. Result should be the minor
    * */
  def getMinor(b: Board): Board = b match {
    case _ :: Nil => Nil
    case _ :: xs => getColumns(xs) match {
      case _ :: ys =>
        if (ys == Nil) Nil
        else getColumns(ys)
      case Nil => xs
    }
    case Nil => Nil
  }

  //returns the first diagonal as a line
  /**
   * Append top left char to getFstDiag(minor) until minor is empty
   */
  def getFstDiag(b:Board): Line = {
    b match {
      case Nil => Nil
      case line :: Nil => line.head :: Nil
      case line :: _ => line.head :: getFstDiag(getMinor(b))
    }
  }

  //returns the second diagonal as a line
  def getSndDiag(b:Board): Line = getFstDiag(mirrorXAxis(b))

  // retrieves all the diagonals above the first line
  def getAboveFstDiag(b: Board): List[Line] = getBelowFstDiag(getColumns(b))

  def getBelowFstDiag(b: Board): List[Line] = {
    def getCorrectSideMinor(b: Board): Board = mirrorXAxis(getMinor(mirrorXAxis(b)))
    def aux(b: Board): List[Line] = {
      if (b == Nil) Nil
      else getFstDiag(b) :: aux(getCorrectSideMinor(b))
    }
    aux(getCorrectSideMinor(b))
  }

  def getAboveSndDiag(b: Board): List[Line] = {
    def getCorrectSideMinor(b: Board): Board = mirrorXYAxis(getMinor(mirrorXYAxis(b)))
    def aux(b: Board): List[Line] = {
      if (b == Nil) Nil
      else getSndDiag(b) :: aux(getCorrectSideMinor(b))
    }
    aux(getCorrectSideMinor(b))
  }

  def getBelowSndDiag(b: Board): List[Line] = {
    def getCorrectSideMinor(b: Board): Board = getMinor(b)
    def aux(b: Board): List[Line] = {
      if (b == Nil) Nil
      else getSndDiag(b) :: aux(getCorrectSideMinor(b))
    }
    aux(getCorrectSideMinor(b))
  }

  //write a function which checks if a given player is a winner
  //hints: patterns and exists
  def winner(p: Player)(b: Board): Boolean = {
    def winner_line(p: Player)(l: Line): Boolean = {
      var consecutiveElems: Int = 0
      for (elem <- l) {
        if (consecutiveElems > 4) return true
        else if (elem == p) consecutiveElems += 1
        else consecutiveElems = 0
      }
      consecutiveElems > 4
    }

    if (winner_line(p)(getFstDiag(b)) ||
      winner_line(p)(getSndDiag(b))) return true

    for (line <- b) if (winner_line(p)(line)) return true
    for (line <- getColumns(b)) if (winner_line(p)(line)) return true

    for (line <- getAboveFstDiag(b)) if (winner_line(p)(line)) return true
    for (line <- getBelowFstDiag(b)) if (winner_line(p)(line)) return true

    for (line <- getAboveSndDiag(b)) if (winner_line(p)(line)) return true
    for (line <- getBelowSndDiag(b)) if (winner_line(p)(line)) return true

    false
  }

  /*
   * Write a function which updates a position (with a player) at given indices from the board.
   * Your function need not check if the position is empty.
   * Partial stub - you can remove it if you want to implement it another way
   */

  def update(p: Player)(y: Int, x: Int, b: Board) : Board = {
    def updateLine(p: Player)(x: Int, l: Line): Line = l match {
      case e :: els =>
        if (x > 0) e :: updateLine(p)(x - 1, els)
        else p :: els
      case _ => Nil
    }
    b match {
      case l :: ls =>
        if (y > 0) l :: update(p)(y - 1, x, ls)
        else updateLine(p)(x, l) :: ls
      case _ => Nil
    }
  }


  /*
   * generates all possible next moves for player p. Hint - use "isFree" and "update"
   *
   * */
  def next(p: Player)(b: Board): List[Board] = {
    @tailrec
    def navigate(p: Player)(curr: Board, acc: List[Board] = Nil, x: Int = 0, y: Int = 0): List[Main.Board] = {
      curr match {
        case Nil :: otherLines => navigate(p)(otherLines, acc, 0, y + 1)
        case line :: otherLines => line match {
          case _ :: els => {
            if (isFree(x, y, b)) navigate(p)(els :: otherLines, update(p)(y, x, b) :: acc, x + 1, y);
            else navigate(p)(els :: otherLines, acc, x + 1, y)
          }
        }
        case Nil => acc
      }
    }
    navigate(p)(b)
  }


  // for testing purposes only.
  def main(args: Array[String]) = {

  }
}

