import scala.List
import scala.annotation.tailrec

trait Nat {}
case object Zero extends Nat {}
case class Succ(n: Nat) extends Nat {}

Succ(Succ(Succ(Zero)))

def fromInt(i: Int): Nat = {
  @tailrec
  def auxConvert(crt: Int, acc: Nat): Nat = {
    if (crt < i) auxConvert(crt + 1, Succ(acc))
    else acc
  }

  auxConvert(0, Zero)
}

fromInt(3)

def toInt(n: Nat): Int = {
  n match {
    case Zero => 0
    case Succ(last) => {
      if (last == Zero) 1
      else 1 + toInt(last)
    }
  }
}

toInt(fromInt(3))

def add(n: Nat, m: Nat): Nat = {

  m match {
    case Zero => n
    case Succ(last) => Succ(add(n, last))
  }
}

toInt(add(fromInt(2), fromInt(3)))

/*
    5.2. Binary Search Trees
 */

trait ITree {}
case object Empty extends ITree
case class INode(key: Int, left: ITree, right: ITree) extends ITree

/*
        5
      /   \
     2     7
    / \     \
   1  3      9
*/
val tree = {
  INode(5,
    INode(2,
      INode(1, Empty, Empty),
      INode(3, Empty, Empty)
    ),
    INode(7,
      Empty,
      INode(9, Empty, Empty)
    )
  )
}

def size(tree: ITree): Int = {
  tree match {
    case Empty => 0
    case INode(_, leftNode, rightNode) =>
      1 + size(leftNode) + size(rightNode)
  }
}

def contains(tree: ITree)(queriedValue: Int): Boolean = {
  tree match {
    case Empty => false
    case INode(value, leftNode, rightNode) =>
      if (value == queriedValue) return true
      else (contains(leftNode)(queriedValue) ||
        contains(rightNode)(queriedValue))
  }
}

contains(tree)(3)
contains(tree)(9)
contains(tree)(124)
contains(tree)(5)

def ins(tree: ITree)(newValue: Int): ITree = {
  tree match {
    case Empty => INode(newValue, Empty, Empty)
    case INode(value, leftNode, rightNode) =>
      if (newValue < value) INode(value, ins(leftNode)(newValue), rightNode)
      else INode(value, leftNode, ins(rightNode)(newValue))
  }
}

ins(tree)(123)
/*
        5
      /   \
     2     7
    / \     \
   1  3      9
              \
              123
*/

ins(tree)(4)
/*
        5
      /   \
     2     7
    / \     \
   1   3     9
        \
         4
*/

List(1,2,3):::List(4,5)

def flatten(tree: ITree): List[Int] = {
  tree match {
    case Empty => Nil
    case INode(v, Empty, Empty) => List(v)
    case INode(v, l, r) =>
      flatten(l) ::: List(v) ::: flatten(r)
  }
}

flatten(tree)

def depth(tree: ITree): Int = {
  tree match {
    case Empty => 0
    case INode(_, Empty, Empty) => 1
    case INode(_, left, right) => 1 + depth(left).max(depth(right))
  }
}

val bigtree = ins(ins(ins(tree)(4))(3))(4)

/*
        5
      /   \
     2     7
    / \     \
   1   3     9
        \
         4
        /
       3
 */

depth(tree)
depth(bigtree)

@tailrec
def minimum(tree: ITree): Int = {
  tree match {
    case Empty => -1
    case INode(v, Empty, Empty) => v
    case INode(_, left, _) => minimum(left)
  }
}

@tailrec
def maximum(tree: ITree): Int = {
  tree match {
    case Empty => -1
    case INode(v, Empty, Empty) => v
    case INode(_, _, right) => maximum(right)
  }
}

/*
        5
      /   \
     2     7
    / \     \
   1  3      9

*/

minimum(tree)
maximum(tree)

//def successor(tree: ITree)(k: Int): Int = {
//  tree match {
//    case Empty => -1
//
//    case INode(k, _, right) => minimum(right)
//    case INode(v, left, right) => {
//      if (k > v) successor(right)(k)
//      else
//      }
//  }
//}
//
///*
//        5
//      /   \
//     2     7
//    / \     \
//   1  3      9
//*/
//
//successor(tree)(2)
//successor(tree)(7)
//successor(tree)(1)
//successor(tree)(9)

