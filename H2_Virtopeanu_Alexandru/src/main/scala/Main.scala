import scala.annotation.tailrec

abstract class WTree extends WTreeInterface {
  def filter(pred: Token => Boolean): WTree = this.filterAux(pred, Empty)
  def filterAux(pred: Token => Boolean, acc: WTree): WTree

  def foldLeft[A](acc: A)(op: (Token, A) => A): A
}

case object Empty extends WTree {
  override def isEmpty = true
  override def ins(w: Token): WTree = Node(w, Empty, Empty)
  override def filterAux(pred: Token => Boolean, acc: WTree): WTree = acc
  override def size: Int = 0
  override def contains(s: String): Boolean = false

  override def foldLeft[A](acc: A)(op: (Token, A) => A): A = acc
}

case class Node(word: Token, left: WTree, right: WTree) extends WTree {
  override def isEmpty = false

  override def ins(w: Token): WTree =
    if (w.freq > word.freq) Node(word, left, right.ins(w))
    else Node(word, left.ins(w), right)

  override def contains(s: String): Boolean = {
    if (word.word == s) true
    else left.contains(s) || right.contains(s)
    // TODO: this could be improved? maybe no need to look either in left or in right
  }
  override def size: Int = 1 + left.size + right.size

  override def filterAux(pred: Token => Boolean, acc: WTree): WTree =
    left.filterAux(pred, right.filterAux(pred, if (pred(this.word)) acc.ins(word) else acc))

  override def foldLeft[A](acc: A)(op: (Token, A) => A): A = this match {
    case Node(tok, Empty, Empty) => op(tok, acc)
    case Node(tok, left, right) => left.foldLeft(right.foldLeft(op(tok, acc))(op))(op)
    case _ => acc
  }
}


object Main {
  def profileID: Int = 754830

  val scalaDescription: String = "Scala is a strong statically typed general-purpose programming language which supports both object-oriented programming and functional programming designed to be concise many of Scala s design decisions are aimed to address criticisms of Java Scala source code can be compiled to Java bytecode and run on a Java virtual machine. Scala provides language interoperability with Java so that libraries written in either language may be referenced directly in Scala or Java code like Java, Scala is object-oriented, and uses a syntax termed curly-brace which is similar to the language C since Scala 3 there is also an option to use the off-side rule to structure blocks and its use is advised martin odersky has said that this turned out to be the most productive change introduced in Scala 3 unlike Java, Scala has many features of functional programming languages like Scheme, Standard ML, and Haskell, including currying, immutability, lazy evaluation, and pattern matching it also has an advanced type system supporting algebraic data types, covariance and contravariance, higher-order types (but not higher-rank types), and anonymous types other features of Scala not present in Java include operator overloading optional parameters named parameters and raw strings conversely a feature of Java not in Scala is checked exceptions which has proved controversial"

  //  split(List('h','i',' ','t','h','e','r','e')) = List(List('h','i'), List('t','h','e','r','e'))
  /* Split the text into chunks */
  def split(text: List[Char]): List[List[Char]] = {
    def aux(text: List[Char]): List[List[Char]] = text match {
      case ' ' :: xs => Nil :: aux(xs)
      case x :: xs => aux(xs) match {
        case y :: ys => (x :: y) :: ys
        case Nil => List(List[Char](x))
      }
      case Nil => Nil
    }

    def trim(list: List[List[Char]]): List[List[Char]] = list match {
      case Nil :: xs => trim(xs)
      case x :: xs => x :: trim(xs)
      case Nil => Nil
    }

    /* if we have only void chunks, we return the empty list */
    val l = trim(aux(text))
    if (l == List(Nil)) Nil
    else l
  }

  def computeTokens(words: List[String]): List[Token] = {
    def insWord(s: String, acc: List[Token]): List[Token] = acc match {
        case x :: xs =>
          if (x.word.equals(s)) Token(s, x.freq + 1) :: xs // s exists, increment freq
          else x :: insWord(s, xs) // check for s in xs
        case Nil => Token(s, 1) :: Nil // reach end so we found a new word
      }

    words.foldLeft(List[Token]())((acc, s) => insWord(s, acc))

      /** replaced by foldLeft call above */
//    @tailrec
//    def aux(rest: List[String], acc: List[Token]): List[Token] = rest match {
//      case word :: xs => aux(xs, insWord(word, acc))
//      case Nil => acc
//    }
  }

  def tokensToTree(tokens: List[Token]): WTree =
    tokens.foldLeft[WTree](Empty)((acc, tok) => acc.ins(tok))

  /* Using the previous function, which builds a tree from a list of tokens,
  *  write a function which takes a string,
  *  splits it into chunks, computes frequencies and constructs a tree.
  *  Use the function _.toList to construct a list of characters from a String.
  *
  *  A much cleaner implementation can be achieved by "sequencing" functions using
  *  andThen.
  * */
  def makeTree(s:String): WTree = {
    def stringify(list: List[List[Char]]): List[String] =
      list.foldLeft(List[String]())((acc, elem) => acc :+ elem.mkString)

    tokensToTree(computeTokens(stringify(split(s.toList))))
  }

  /* build a tree with the words and frequencies from the text in the scalaDescription text */
  def wordSet: WTree = makeTree(scalaDescription)

  /* find the number of occurrences of the keyword "Scala" in the scalaDescription text */
  def scalaFreq: Int = wordSet.filter((tok: Token) => tok.word.equals("Scala")) match {
    case Node(token, Empty, Empty) => token.freq
    case Node(_, _, _) => -1 // multiple tokens with "Scala" make no sense
    case Empty => 0 // "Scala" not found
  }

  /* find how many programming languages are referenced in the text.
    *   A PL is a keyword which starts with an uppercase
    *   You can reference a character from a string using (0) and you can
    *   also use the function isUpper
    */
  def progLang: Int = wordSet.filter((tok: Token) => tok.word(0).isUpper).size

  /* find how many words which are not prepositions or conjunctions appear in the text (any word whose size is larger than 3). */
  def wordCount : Int = wordSet.filter((tok: Token) => tok.word.length > 3)
    .foldLeft(0)((tok, sum) => sum + tok.freq)
}

