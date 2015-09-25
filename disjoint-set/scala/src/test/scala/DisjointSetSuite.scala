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
}
