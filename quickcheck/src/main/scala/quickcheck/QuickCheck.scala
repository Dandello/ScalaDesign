package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop.{forAll, BooleanOperators}
//import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(i, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  def getElemSeq(heap: H):List[A] = heap match {
    case heap if isEmpty(heap) => Nil
    case x :: xs => findMin(heap) :: getElemSeq(deleteMin(heap))
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min of two elements") = forAll {
    (a: Int, b:Int) => (a < b) ==> {
      val heap = insert(b, insert(a, empty))
      findMin(heap) == a
    }
  }

  property("delete elem") = forAll {
    elem:Int => deleteMin(insert(elem, empty)) == empty
  }

  property("sorted sequence") = forAll {
    (heap: H) => {
      val sortedSeq = getElemSeq(heap)
      (sortedSeq, sortedSeq.tail).zipped.forall(_ <= _)
    }
  }

  property("min of melding heaps") = forAll {
    (firstHeap: H, secondHeap: H) =>
      (!isEmpty(firstHeap) && !isEmpty(secondHeap)) ==> {
        val meldHeap = meld(firstHeap, secondHeap)
        val min = findMin(meldHeap)
        min == findMin(firstHeap) || min == findMin(secondHeap)
      }
  }

  property("associative meld") = forAll {
    (firstHeap: H, secondHeap: H, thirdHeap: H) =>
      getElemSeq(meld(firstHeap, meld(secondHeap, thirdHeap))) == getElemSeq(meld(meld(firstHeap, secondHeap), thirdHeap))
  }

  property("double insert and remove") = forAll {
    (a:Int, b:Int) =>
      val heap = insert(a, insert(b, empty))
      deleteMin(deleteMin(heap)) == empty
  }



}
