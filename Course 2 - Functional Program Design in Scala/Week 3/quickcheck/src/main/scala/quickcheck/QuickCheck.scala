package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
      x  <- arbitrary[A]
      ts <- frequency((1, Gen.const(empty)), (9, genHeap))
  } yield insert(x, ts)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
      val h = insert(a, empty)
      findMin(h) == a
  }

  property("If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.") = {
      forAll { (firstValue: Int, secondValue: Int) =>
          val firstHeap = insert(firstValue, empty)
          val secondHeap = insert(secondValue, firstHeap)
          val minInsertedValue = Math.min(firstValue, secondValue)

          findMin(secondHeap) == minInsertedValue
      }
  }

  property("If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.") = {
      forAll { (insertionValue: Int) =>
          val heapWithInsertion = insert(insertionValue, empty)
          val heapWithDeletion = deleteMin(heapWithInsertion)
          heapWithDeletion == empty
      }
  }

  property("Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima.") = {
      forAll { (h: H) =>
          var minList: List[Int] = Nil
          var heap: H = h

          while (!isEmpty(heap)) {
              var minHeapValue: Int = findMin(heap)
              minList = minList ++ List(minHeapValue)
              heap = deleteMin(heap)
          }
          minList == minList.sorted
      }
  }


  property("Finding a minimum of the melding of any two heaps should return a minimum of one or the other.") = {
      forAll { (heap1: H, heap2: H) =>

          val heap1Min = findMin(heap1)
          val heap2Min = findMin(heap2)
          val globalMin = Math.min(heap1Min, heap2Min)

          val mergedHeap = meld(heap1, heap2)
          val mergedHeapMin = findMin(mergedHeap)

          mergedHeapMin == globalMin
      }
  }

  property("Transferring nodes between heaps should not affect final union") = forAll { (heap1: H, heap2: H) =>
      def equalHeaps(heap1: H, heap2: H): Boolean =
          if (isEmpty(heap1) && isEmpty(heap2)) true
          else {
              val heap1Min = findMin(heap1)
              val heap2Min = findMin(heap2)
              heap1Min == heap2Min && equalHeaps(deleteMin(heap1), deleteMin(heap2))
          }

      val mergedHeaps: H = meld(heap1, heap2)

      val heap1Min = findMin(heap1)
      val heap1WithoutMin = deleteMin(heap1)
      val heap2WithHeap1Min = insert(heap1Min, heap2)
      val mergedHeapsWithTransfer = meld(heap1WithoutMin, heap2WithHeap1Min)

      equalHeaps(mergedHeaps, mergedHeapsWithTransfer)
  }












}
