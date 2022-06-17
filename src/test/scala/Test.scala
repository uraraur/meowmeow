import munit.ScalaCheckSuite
import org.scalacheck._
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary.arbitrary
import postmodern1._
import scala.util.Random

class IntegerSuite extends ScalaCheckSuite {

  given [A: Arbitrary]: Arbitrary[List[A]] =
    Arbitrary(Gen.listOf(arbitrary[A]).map(List(_*)))

  property("Reverse idempotency") {
    forAll { (n1: List[Int]) =>
      n1.reverse.reverse == n1
    }
  }
  property("Intersect with itself is that same list") {
    forAll{ (n1: List[Int]) =>
      List.intersect(n1, n1) == n1.reverse
    }
  }
  property("Intersect with empty list is empty list") {
    forAll{ (n1: List[Int]) =>
      List.intersect(n1, List.Nil) == List.Nil
    }
  }
   property("Intersection of disjoint lists") {
    val n1: List[Int] = List(1, 2, 3)
    val n2: List[Int] = List(4, 5, 6)
      List.intersect(n1, n2) == List.Nil
  }
  property("Intersection") {
    val n1: List[Int] = List(1, 2, 3, 4, 5, 6)
    val n2: List[Int] = List(4, 5, 6, 9)
      List.intersect(n1, n2) == List(6, 5, 4)
  }
  property("IntersectionBy") {
    val n1: List[Int] = List(2, 3, 4, 9, 16)
    val n2: List[Int] = List(1, 4, 5, 16, 25, 0)
      List.intersectBy(n1, n2, x => x*x) == List(4, 2)
  }
  property("Spans") {
    val n1: List[Int] = List(1, 0, 2, 0, 3, 4, 5, 6, 0)
      List.spans(n1, 0) == List(List(1),List(2),List(3,4,5,6),List.Nil)
  }
  property("Spans1") {
    val n1: List[Int] = List(0)
      List.spans(n1, 0) == List(List.Nil, List.Nil)
  }
}