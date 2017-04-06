package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      n <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(n, h)
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }


  property("insertAndRemove") = forAll { a: Int =>
    val h = deleteMin(insert(a, empty))
    isEmpty(h)
  }

  property("minimum of two") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == a.min(b)
  }

  property("minimum commutes with meld") = forAll {(h1: H, h2: H) =>
    val h = meld(h1,h2)
    (isEmpty(h1), isEmpty(h2)) match {
      case (true,true) => true
      case (true,false) => findMin(h) == findMin(h2)
      case (false,true) => findMin(h) == findMin(h1)
      case (false,false) => findMin(h) == findMin(h1).min( findMin(h2) )
    }
  }
}
