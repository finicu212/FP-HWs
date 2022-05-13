def atLeastk(k: Int, l: List[Int]): Boolean = {
  if (k == 0) true
  else l match {
    case Nil => false
    case _ :: xs => atLeastk(k - 1, xs)
  }
}

var l1 = List(7, 8, 9, 10, 11)
atLeastk(5, l1)
atLeastk(6, l1)

var retList = List()

def take(n: Int, l: List[Int]): List[Int] = {
  if (n == 0) Nil
  else l match {
    case x :: xs => x :: take(n - 1, xs)
    case Nil => Nil
  }
}

take(2, l1)

def drop(n: Int, l: List[Int]): List[Int] = {
  if (n == 0) Nil
  else l match {
    case _ :: xs => drop(n - 1, xs)
    case Nil => Nil
  }
}

l1.drop(2)
l1.drop(3)

def takeP(p: Int => Boolean)(l: List[Int]): List[Int] = l match {
  case x :: xs =>
    if (p(x)) x :: takeP(p)(xs)
    else takeP(p)(xs)
  case Nil => Nil
}

var isEven = (x: Int) => x % 2 == 0
takeP(isEven)(List(1,2,3,4,5,6))

def part(p: Int => Boolean)(l: List[Int]): (List[Int], List[Int]) = l match {
  case x :: xs =>
    val pair = part(p)(xs)
    if (p(x)) (x :: pair._1, pair._2)
    else (pair._1, x :: pair._2)

  case Nil => (Nil, Nil)
}

part(isEven)(List(1,2,3,4,5,6))

type Gradebook = List[(String,Int)] //the type Gradebook now refers to a list of pairs of String and Int
val gradebook = List(("G",3), ("F", 10), ("M",6), ("P",4))

// Write a function which adds one point to all students which have a passing grade (>= 5), and leaves all other grades unchanged.
def increment(g: Gradebook): Gradebook = {
  def incrementGradeIfPassing(student: String, grade: Int): (String, Int) = {
    if (grade >= 5) (student, grade + 1)
    else (student, grade)
  }

  g.map(p =>
    if (p._2 >= 5) (p._1, p._2 + 1)
    else (p._1, p._2)
  )

  g.map(p => incrementGradeIfPassing(p._1, p._2))
}

increment(gradebook)
gradebook.tail

def average(g: Gradebook): Double = {
  0.0
  //g.foldRight(g.tail)(((_, x), (_, y)) => x + y)
}