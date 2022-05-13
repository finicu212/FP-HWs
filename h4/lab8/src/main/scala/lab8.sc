import java.lang.Math.pow

case class Gradebook(book: Map[String,Int]) {
  def + (entry: (String, Int)): Gradebook = Gradebook(book + entry)

  def setGrade(name: String, newGrade: Int): Gradebook = Gradebook(book + (name -> newGrade))

  //def ++(other: Gradebook): Gradebook = book ++ other

  def ++(other: Gradebook): Gradebook = {
    def updateBook(book: Map[String,Int], pair: (String,Int)): Map[String,Int] =
      if (book contains pair._1)
        if (book(pair._1) < pair._2) book + pair
        else book
      else book + pair

    Gradebook(other.book.foldLeft(book)(updateBook))
  }

  def gradeNo: Map[Int,Int] = {
    def setOccurrence(acc: Map[Int, Int], pair: (String,Int)): Map[Int,Int] = {
      // increment occurrence
      if (acc contains pair._2) acc + (pair._2 -> (acc(pair._2) + 1))
      // set occurrence to 1, first time we see this grade
      else acc + (pair._2 -> 1)
    }

    book.foldLeft(Map[Int, Int]())(setOccurrence)
  }
}

var x = Gradebook(Map("Alex" -> 1))
x = x + ("John" -> 2)
x = x + ("Zulu" -> 10)
x = x + ("Xohn" -> 6)

var y = Gradebook(Map("Alex" -> 5))
y = y + ("John" -> 6)

var z = x ++ y

z.gradeNo

// Polynomials --- - ---
case class Polynomial (nonZeroTerms: Map[Int,Int]) {
  def *(n: Int): Polynomial = Polynomial(
    nonZeroTerms.foldLeft(Map[Int, Int]())((acc, kv) =>
      acc + (kv._1 -> (kv._2 * n))
    )
  )

  override def toString: String = nonZeroTerms.foldLeft("")((acc, kv) =>
    if (kv._1 > 0) acc + kv._2 + "*X^" + kv._1 + " + "
    else acc + kv._2 + "*X^" + kv._1)

  def +(p2: Polynomial): Polynomial = {
    def update(acc: Map[Int, Int], pair: (Int, Int)): Map[Int, Int] =
      if (acc contains pair._1)
        acc + (pair._1 -> (pair._2 + acc(pair._1)))
      else acc + pair

    Polynomial(p2.nonZeroTerms.foldLeft(nonZeroTerms)(update))
  }

  def hasRoot(r: Int): Boolean = {
    val substitutedResult = nonZeroTerms.foldLeft(0)((acc, kv) => acc + kv._2 * (pow(r, kv._1)).toInt)
    if (substitutedResult == 0) true else false
  }
}

var polynomial = Polynomial(Map(2 -> 2, 0 -> 3)) // encodes 2*X^2 + 3
polynomial.toString
polynomial + polynomial
polynomial * 3

var polynomial2 = Polynomial(Map(1 -> 3, 0 -> 3)) // encodes 3*X + 3

polynomial2.hasRoot(-1)