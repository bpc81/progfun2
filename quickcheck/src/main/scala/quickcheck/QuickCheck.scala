package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.annotation.tailrec

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
/*
  def isIncreasing(h: H): Boolean = {
    if (isEmpty(h)) true else {
      val myMin = findMin(h)
      val hh = deleteMin(h)
      if (isEmpty(hh)) true else (myMin <= findMin(hh)) && isIncreasing(hh)
    }
  }

  property("alternate calculation of increasing sequence") = forAll {(h:H) =>
    isIncreasing(h)
  }
*/

  /*
  def compareWithPrev(prev: Int, h: H): Boolean = {
    if (isEmpty(h)) true else {
      val next = findMin(h)
      (prev <= next) && compareWithPrev(next, deleteMin(h))
    }
  }

//  property("repeatedly extracting minima yields increasing sequence") = forAll { (h: H) =>
//    compareWithPrev(Int.MinValue, h)
//  }
*/

  property("deleteMin removes correct element") = forAll { (h: H) =>
    if (isEmpty(h)) true else {
      val n = findMin(h)
      val m = n.min(n-1) // catches underflow
      val g = deleteMin( insert(m, h) )
      findMin(g) == n
    }
  }

  def count(h: H): Int = if (isEmpty(h)) 0 else 1 + count(deleteMin(h))
  property("deleteMin reduces size by one") = forAll { (h: H) =>
    isEmpty(h) || count(h) == 1 + count(deleteMin(h))
  }

  property("delete and insert") = forAll { (h: H) =>
    isEmpty(h) || {
      val myMin = findMin(h)
      val g = insert(myMin, deleteMin(h))
      findMin(g) == myMin
    }
  }


  property("construct from sequence & find min") = forAll { (ns: Seq[Int]) =>
    val h: H = ns.foldRight(empty)( insert(_,_) )
    (ns.length == 0 || findMin(h) == ns.min)
  }

  def collect(h: H): List[Int] = if (isEmpty(h)) Nil else findMin(h) :: collect(deleteMin(h))
  property("collect returns increasing sequence") = forAll{ (h: H) => {
    val data = collect(h)
    data == data.sorted
    }
  }

  property("preserves maximum") = forAll { (h: H, a: Int) =>
    collect( insert(a,h) ).max >= a
  }



}
