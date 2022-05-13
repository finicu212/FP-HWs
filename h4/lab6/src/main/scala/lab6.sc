trait FList[A]{ // list with elements of type A
  def length: Int
  def head: A
  def tail: FList[A]
  def map[B](f: A => B): FList[B]
  // a op (b op (c op acc))
  def foldRight[B](acc: B)(op: (A,B) => B): B
  // ((acc op a) op b) op c
  def foldLeft[B](acc: B)(op: (B,A) => B): B
  def contains(e: A):Boolean =
    this.foldRight(false)(_ == e || _)
  def indexOf(e: A): Int
  // TODO: try map() with this
  def update(e: A, pos: Int): FList[A]
  def append(l: FList[A]): FList[A] = this.foldRight(l)(Cons(_, _))
  def reverse: FList[A]
  def last: A
  def filter(p: A => Boolean): FList[A]

  // Cons(1,(Cons(2,Cons(3,FNil()))).zip(Cons(true,Cons(false,Cons(true,FNil())))) =
  // Cons((1,true),Cons((2,false),Cons((3,true),FNil())))
  def zip[B](l: FList[B]): FList[(A,B)]
}

/*
*   List(a, b, c).foldRight(acc)(op)
*   a op (b op (c op acc))
*
*   List(1, 2, 3) ++ List(4, 5)
*   Cons(1, Cons(2, Cons(3, l))
* */

case class FNil[A]() extends FList[A]{
  override def length: Int = 0
  override def head: A = throw new Exception("head on empty list")
  override def tail: FList[A] = throw new Exception("head on empty list")
  override def map[B](f: A => B): FList[B] = FNil[B]()
  override def foldRight[B](acc: B)(op: (A,B) => B): B = acc
  override def foldLeft[B](acc: B)(op: (B,A) => B): B = acc
  override def indexOf(e: A): Int = -1
  override def update(e: A, pos: Int): FList[A] = FNil[A]()
  override def reverse: FList[A] = FNil[A]()
  override def last: A = throw new Exception("last on empty list")
  override def filter(p: A => Boolean): FList[A] = FNil[A]()
  override def zip[B](l: FList[B]): FList[(A,B)] = FNil[(A, B)]()
}

case class Cons[A](x:A, xs:FList[A]) extends FList[A]{
  override def length = 1 + xs.length
  override def head:A = x
  override def tail:FList[A] = xs
  override def map[B](f: A => B): FList[B] =
    Cons(f(x),xs.map(f))
  override def foldRight[B](acc: B)(op: (A,B) => B): B =
    op(x, xs.foldRight(acc)(op))
  override def foldLeft[B](acc: B)(op: (B,A) => B): B =
    xs.foldLeft(op(acc,x))(op)
  override def indexOf(e: A): Int = {
    if (x == e) 0
    else 1 + xs.indexOf(e)
  }
  override def update(e: A, pos: Int): FList[A] = {
    if (pos == 0) Cons(e, xs)
    else Cons(x, xs.update(e, pos - 1))
  }
  //override def append(l: FList[A]): FList[A] = Cons(x, xs.append(l))

  /**
   * reverse(xs) ++ x
   * to reverse
   */
  override def reverse: FList[A] = xs.reverse.append(Cons(x, FNil()))

  override def last: A =
    if (xs == FNil[A]()) x
    else xs.last

  override def filter(p: A => Boolean): FList[A] =
    if (p(x)) Cons(x, xs.filter(p))
    else xs.filter(p)

  override def zip[B](l: FList[B]) = {


  }
}

val myList = Cons(1, Cons(2, Cons(3, Cons(4, FNil()))))

myList.indexOf(3)
myList.update(4, 1)
myList.append(Cons(4, FNil()))
myList.reverse
myList.last
myList.filter((x: Int) => x % 2 == 0)