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
  property("Spans1") {
    val n1: List[Int] = List(0)
      List.spans(n1, 0) == List(List.Nil, List.Nil)
  }
  property("Intersection is a subset of both sets"){
    forAll{ (n1: List[Int], n2: List[Int]) =>
      var temp: List[Int] = List.intersect(n1, n2)
      var m: Boolean = true
      while temp != List.Nil do
        temp match{
          case List.Cons(h, t) => m = m && n2.contains(h) && n1.contains(h) 
          temp = t
        }
      m
    }   
  }
}