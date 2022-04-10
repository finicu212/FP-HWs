trait FSets {

  def profileID:Int = 754830

  /* write a function which takes an integer and returns the set containing that integer:
   */
  def singleton(x: Int): Int => Boolean = {
    (elem: Int) => x == elem
  }

  /* write a function which takes a set and an integer, and returns true if the integer is a member of the set.
     The function should be curried.
   */
  def member(set: Int => Boolean)(e: Int): Boolean = {
    if (set(e)) true
    else false
  }

  /* write a function which takes two integer bounds, and returns the set containing all integers
     between the bounds. Hint: you can use a tail-recursive function together with singleton, but there is
     also a simpler way.
   */
  def fromBounds(start: Int, stop: Int): Int => Boolean = {
    (elem: Int) => (elem >= start && elem <= stop)
  }

  /* write intersection and reunion of two sets */

  def intersection(set1: Int => Boolean, set2: Int => Boolean): Int => Boolean = {
    (elem: Int) => member(set1)(elem) && member(set2)(elem)
  }

  def union(set1: Int => Boolean, set2: Int => Boolean): Int => Boolean = {
    (elem: Int) => set1(elem) || set2(elem)
  }

  /* write a function which takes bounds, as well as a set, and computes the sum of all
     elements from the set. Use inner (tail-recursive) functions
   */

  def sumSet(start: Int, stop: Int, set: Int => Boolean): Int = {
    def auxSum(crt: Int, acc: Int): Int = {
      if (crt > stop) acc
      else if (set(crt)) auxSum(crt + 1, acc + crt)
      else auxSum(crt + 1, acc)
    }

    auxSum(start, 0)
  }

  /* generalise the previous function such that we can fold the set using any binary commutative
     operation over integers;
   */
  def foldSet(
               start: Int,            // bounds (inclusive)
               stop: Int,
               op: (Int, Int) => Int, // folding operation
               initial: Int,          // initial value
               set: Int => Boolean    // the set to be folded
             ): Int = {
    def tail_fold(crt: Int, acc: Int): Int = {
      if (crt > stop) acc
      else if (set(crt)) tail_fold(crt + 1, op(acc, crt))
      else tail_fold(crt + 1, acc)
    }

    tail_fold(start, initial)
  }

  /* implement a function forall which checks if all elements in a given range of a set satisfy a condition */

  def forall(
              start: Int, // start value (inclusive)
              stop: Int, // stop value (inclusive)
              condition: Int => Boolean, // condition to be checked
              set: Int => Boolean // set to be checked
            ): Boolean = {
    def tail_for(crt: Int, flag: Boolean): Boolean = {
      if (crt > stop) flag
      else if (!member(set)(crt)) tail_for(crt + 1, flag)
      else if (condition(crt)) tail_for(crt + 1, flag)
      else false
    }

    tail_for(start, true)
  }


  /* implement a function exists, using forall */
  def exists(
              start: Int, // start value (inclusive)
              stop: Int, // stop value (inclusive)
              condition: Int => Boolean, // condition to be checked
              set: Int => Boolean // set
            ): Boolean = {
    def tail_for(crt: Int, flag: Boolean): Boolean = {
      if (crt > stop) flag
      else if (!member(set)(crt)) tail_for(crt + 1, flag)
      else if (!condition(crt)) tail_for(crt + 1, flag)
      else true
    }

    tail_for(start, false)
  }

}


object FSets extends FSets
