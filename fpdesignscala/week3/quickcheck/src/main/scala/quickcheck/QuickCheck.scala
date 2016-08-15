package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    value <- arbitrary[Int]
    heap <- oneOf(const(empty), genHeap)
  } yield insert(value, heap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("Inserting on a empty heap = non empty heap") = forAll { (value: Int) =>
    !isEmpty(insert(value, empty))
  }

  property("Inserting on a empty heap and deleting = empty heap") = forAll { (value: Int) =>
    isEmpty(deleteMin(insert(value, empty)))
  }

  property("Inserting on a arbitrary heap = non empty heap") = forAll { (heap: H, value: Int) =>
    !isEmpty(insert(value, heap))
  }

  property("Inserting on a arbitrary heap and deleting = empty heap") = forAll { (heap: H, value: Int) =>
    isEmpty(deleteMin(insert(value, empty)))
  }

  property("If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the " +
    "smallest of the two elements back") = forAll { (value1: Int, value2: Int) =>
    val heap1 = insert(value2, insert(value1, empty))
    val heap2 = insert(value1, insert(value2, empty))

    val min1 = findMin(heap1)
    val min2 = findMin(heap2)

    min1 == min2 && min1 == Math.min(value1, value2)
  }

  property("Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima. " +
    "(Hint: recursion and helper functions are your friends.)") = forAll { (heap: H) =>
    val heapList = heapToList(heap)
    heapList == heapList.sorted
  }

  property("Finding a minimum of the melding of any two heaps should return a minimum of one or the other")
    = forAll { (heap1: H, heap2: H) =>
    val melded1 = meld(heap1, heap2)
    val melded2 = meld(heap2, heap1)

    val min1 = findMin(melded1)
    val min2 = findMin(melded2)

    min1 == min2 && min1 == Math.min(findMin(heap1), findMin(heap2))
  }

  property("Melding two heaps should be a commutative")
    = forAll { (heap1: H, heap2: H) =>
    val melded1 = meld(heap1, heap2)
    val melded2 = meld(heap2, heap1)

    heapEqual(melded1, melded2)
  }

  property("Molding three heaps should be associative") = forAll { (heap1: H, heap2: H, heap3: H) =>
    val melded1 = meld(meld(heap1, heap2), heap3)
    val melded2 = meld(heap1, meld(heap2, heap3))

    heapEqual(melded1, melded2)
  }


  property("Removing min and adding to other heap gives back some heap when melded") = forAll { (heap1: H, heap2: H) =>
    val min1 = findMin(heap1)
    val min2 = findMin(heap2)

    val melded = meld(heap1, heap2)

    val melded12 = meld(deleteMin(heap1), insert(min1, heap2))
    val melded21 = meld(deleteMin(heap2), insert(min2, heap1))

    heapEqual(melded12, melded21) && heapEqual(melded, melded12)
  }


  def heapToList(h: H): List[Int] = {
    def iter(remaining: H, acc: List[Int]): List[Int] = {
      if (isEmpty(remaining)) acc.reverse
      else iter(deleteMin(remaining), findMin(remaining) :: acc)
    }
    iter(h, Nil)
  }

  def heapEqual(h1: H, h2: H) = {
    heapToList(h1) == heapToList(h2)
  }


}
