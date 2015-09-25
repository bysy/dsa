import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSuite, Matchers, Tag}

/**
 * Created by Benjamin on 9/25/15.
 */
class DisjointSetSuite extends FunSuite
  with GeneratorDrivenPropertyChecks with Matchers {

  val smallPositiveInts = for {n <- Gen.choose(0,1000)} yield n
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
}