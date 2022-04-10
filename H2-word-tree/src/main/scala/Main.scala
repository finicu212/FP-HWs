import scala.annotation.tailrec

abstract class WTree extends WTreeInterface {
  override def filter(pred: Token => Boolean): WTree = this.filterAux(pred, Empty)
  def filterAux(pred: Token => Boolean, acc: WTree): WTree
}

case object Empty extends WTree {
  override def isEmpty = true
  override def ins(w: Token): WTree = Node(w, Empty, Empty)
  override def filterAux(pred: Token => Boolean, acc: WTree): WTree = Empty
  override def size: Int = 0
  override def contains(s: String): Boolean = false
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

  override def filterAux(pred: Token => Boolean, acc: WTree): WTree = {
    val aux_acc = if (pred(word)) acc.ins(word) else acc
    left.filterAux(pred, right.filterAux(pred, aux_acc))
  }
}


object Main {

  def profileID: Int = 754830

  val tree: WTree = Node(Token("Hello", 2), Empty, Empty)
  val newTree: WTree = tree.ins(Token("World", 3)).ins(Token("There", 1))

  println(newTree.toString)
  println(newTree.filter(x => {x.freq % 2 == 1}))

  val scalaDescription: String = "Scala is a strong statically typed general-purpose programming language which supports both object-oriented programming and functional programming designed to be concise many of Scala s design decisions are aimed to address criticisms of Java Scala source code can be compiled to Java bytecode and run on a Java virtual machine. Scala provides language interoperability with Java so that libraries written in either language may be referenced directly in Scala or Java code like Java, Scala is object-oriented, and uses a syntax termed curly-brace which is similar to the language C since Scala 3 there is also an option to use the off-side rule to structure blocks and its use is advised martin odersky has said that this turned out to be the most productive change introduced in Scala 3 unlike Java, Scala has many features of functional programming languages like Scheme, Standard ML, and Haskell, including currying, immutability, lazy evaluation, and pattern matching it also has an advanced type system supporting algebraic data types, covariance and contravariance, higher-order types (but not higher-rank types), and anonymous types other features of Scala not present in Java include operator overloading optional parameters named parameters and raw strings conversely a feature of Java not in Scala is checked exceptions which has proved controversial"

  //  split(List('h','i',' ','t','h','e','r','e')) = List(List('h','i'), List('t','h','e','r','e'))
  /* Split the text into chunks */
  def split(text: List[Char]): List[List[Char]] = {
    @tailrec
    def aux(text: List[Char], prevList: List[Char] = Nil, acc: List[List[Char]] = Nil): List[List[Char]] =
      if ((text == Nil || text.head == ' ')
        && prevList == Nil
        && acc == Nil) Nil
      else text match {
        case Nil => acc :+ prevList
        case ' ' :: xs => aux(xs, Nil, acc :+ prevList)
        case x :: xs => aux(xs, prevList :+ x, acc)
      }

    def trim(list: List[List[Char]]): List[List[Char]] = list match {
      case Nil :: xs => trim(xs)
      case x :: xs => x :: trim(xs)
      case Nil => Nil
    }

    // if we have only void chunks, we return the empty list
    val l = trim(aux(text))
    if (l == List(Nil)) Nil
    else l
  }

  /*
  /* compute the frequency of each chunk */
  def computeTokens(words: List[String]): List[Token] = {
    /* insert a new string in a list of tokens */
    def insWord(s: String, acc: List[Token]): List[Token] = ???

    /* tail-recursive implementation of the list of tokens */
    def aux(rest: List[String], acc: List[Token]): List[Token] = ???

    ???
  }
   */
  def computeTokens(words: List[String]): List[Token] = {
    def insWord(s: String, acc: List[Token]): List[Token] = acc match {
        case x :: xs =>
          if (x.word.equals(s)) Token(s, x.freq + 1) :: xs // s exists, increment freq
          else x :: insWord(s, xs) // check for s in xs
        case Nil => Token(s, 1) :: Nil // reach end so we found a new word
      }

    @tailrec
    def aux(rest: List[String], acc: List[Token]): List[Token] = rest match {
      case word :: xs => aux(xs, insWord(word, acc))
      case Nil => acc
    }

    aux(words, Nil)
  }

  def tokensToTree(tokens: List[Token]): WTree = ???

  /* Using the previous function, which builds a tree from a list of tokens,
  *  write a function which takes a string,
  *  splits it into chunks, computes frequencies and constructs a tree.
  *  Use the function _.toList to construct a list of characters from a String.
  *
  *  A much cleaner implementation can be achieved by "sequencing" functions using
  *  andThen.
  * */

  def makeTree(s:String): WTree = ???

  /* build a tree with the words and frequencies from the text in the scalaDescription text */
  def wordSet: WTree = ???

  /* find the number of occurrences of the keyword "Scala" in the scalaDescription text */
  def scalaFreq: Int = ???

  /* find how many programming languages are referenced in the text.
     A PL is a keyword which starts with an uppercase
     You can reference a character from a string using (0) and you can
     also use the function isUpper

  */
  def progLang: Int = ???

  /* find how many words which are not prepositions or conjunctions appear in the text (any word whose size is larger than 3). */

  def wordCount : Int = ???


}

