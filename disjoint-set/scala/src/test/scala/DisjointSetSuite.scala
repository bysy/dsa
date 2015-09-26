import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSuite, Matchers, Tag}

/**
 * Test DisjointSet.
 */
class DisjointSetSuite extends FunSuite
  with GeneratorDrivenPropertyChecks with Matchers {

  val smallPositiveInts = for {n <- Gen.choose(0,1000)} yield n

  val djSet: Gen[DisjointSet] = {
    for {
      size <- Gen.choose(1,30)
      set = DisjointSet(size)
      numU <- Gen.choose(0,2*size)
      pair = for { a <- Gen.choose(0,size-1)
                   b <- Gen.choose(0,size-1) } yield (a,b)
      unions <- Gen.containerOfN[List,(Int,Int)](numU, pair)
    } yield unions.toArray.foldLeft(set) {
      case (s,(i,j)) => s.union(i,j)
    }
  }
  val creationTag = Tag("creation")

  test("initial state array has expected length", creationTag) {
    forAll (smallPositiveInts) { n =>
      DisjointSet(n).size shouldBe n
    }
  }
  test("each initial element is its own root", creationTag) {
    forAll (smallPositiveInts) { n =>
      DisjointSet(n).numGroups shouldBe n
    }
  }
  test("union/areConnected example") {
    val set = DisjointSet(10)
      .union(0,1).union(2,1).union(8,9).union(2,8)
    assert(set.areConnected(0,1))
    assert(set.areConnected(0,2))
    assert(set.areConnected(0,9))
    assert(set.areConnected(1,8))
    assert(!set.areConnected(0,5))
    assert(!set.areConnected(5,6))
    assert(!set.areConnected(6,9))
  }
  test("areConnected doesn't change result") {
    forAll (djSet, smallPositiveInts, smallPositiveInts) { (djs,a_,b_) =>
      val (a,b) = (a_ % djs.size, b_ % djs.size)
      val origv = djs.areConnected(a, b)
      assert(origv == djs.areConnected(a, b))
      assert(origv == djs.areConnected(a, b))
    }
  }
  test("root doesn't change result") {
    forAll (djSet, smallPositiveInts) { (djs, a_) =>
      val a = a_ % djs.size
      val origv = djs.root(a)
      assert(origv == djs.root(a))
      assert(origv == djs.root(a))
      assert(origv == djs.root(a))
    }
  }
}
