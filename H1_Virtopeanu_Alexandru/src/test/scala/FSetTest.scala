import FSets._

class FSetTest extends munit.FunSuite {

  val emptySet : Int => Boolean = x => false


  test("Valid profile id:"+profileID){
    assert(profileID > 0)
  }
  // (10p) 1. Member - 3 tests
  test("Empty set (3p):") {
    assert(!(member(x => false)(10)))
  }

  test("Singleton member test 1 (3p)") {
    val s = singleton(10)
    assert(member(s)(10))
  }

  test("Singleton member test 2 (4p)") {
    val s = singleton(10)
    assert(!member(s)(5))
  }
  // (11p) 2. fromBounds - 1 test
  test("fromBounds (11p) ") {
    val three = fromBounds(1,3)
    assert(member(three)(1) && member(three)(2) && member(three)(3))
  }
  // (12p) 3. intersection - 2 tests
  test("Intersection with empty set (6p)") {
    val s = singleton(10)
    assert(!member(intersection(s,emptySet))(10))
  }

  test("Intersection with singleton set (6p)"){
    val s = singleton(10)
    assert(member(intersection(s,singleton(10)))(10))
  }
  // (12p) 4. union - 3 tests
  test("Union with empty set (4p) ") {
    val s = singleton(10)
    assert(member(union(s, emptySet))(10))
  }

  test("Union with two singleton sets, lhs (4p)") {
    val s = singleton(10)
    assert(member(union(s, singleton(5)))(5))
  }

  test("Union with two singleton sets, rhs (4p)") {
    val s = singleton(10)
    assert(member(union(s,singleton(5)))(10))
  }
  // (10p) 5. sumSet - 2 tests
  test("sumSet with empty set (5p)") {
    assert(sumSet(5, 10, emptySet) == 0)
  }

  test("sumSet with bounds (5p)"){
    val set = fromBounds(1, 20)
    assert(sumSet(5,10,set) == 45)
  }
  // (15p) 6. foldSet - 3 tests
  test("foldSet with empty set (5p)") {
    assert(foldSet(5, 10, _ + _, 0, emptySet) == 0)
  }

  test("foldSet with addition (5p)") {
    val set = fromBounds(1, 20)
    assert(foldSet(5, 10, _ + _, 0, set) == 45)
  }

  test("foldSet with multiplication (5p)") {
    val set = fromBounds(1, 20)
    assert(foldSet(5,10,_ * _, 1, set) == 151200)

  }
  // (15p) 7. forall - 3 tests
  test("Forall with empty set (5p)"){
    assert(forall(0,100,x => false, emptySet))
  }

  test("Forall with all even numbers (5p)") {
    val evens: Int => Boolean = x => x % 2 == 0
    assert(forall(0, 100, x => x % 2 == 0, evens))
  }

  test("Forall with one odd number (5p)") {
    val evens: Int => Boolean = x => x % 2 == 0
    assert(!forall(0,100, x => x % 2 == 0, union(evens,singleton(99))))
  }
  // (15p) 8. exists - 3 tests
  test("Exists with empty set (5p)") {
    assert(!exists(0, 100, x => true, emptySet))
  }

  test("Exists odd number in set of all-even numbers (5p)") {
    val evens: Int => Boolean = x => x % 2 == 0
    assert(!exists(0, 100, x => x % 2 == 1, evens))
  }

  test("Exists - odd number in set with one odd number (5p)") {
    val evens: Int => Boolean = x => x % 2 == 0
    assert(exists(0,100, x => x % 2 == 1, union(evens,singleton(99))))
  }

}

