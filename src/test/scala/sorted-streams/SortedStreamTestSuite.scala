package sortstreams

import org.scalatest.FunSuite

class SortedStreamTestSuite extends FunSuite {
  import SortedStreamExamples._
  val nats = intsFrom(0)
  val ten = nats.take(10)

  test("ten first natural numbers") {
    ten.toList == List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  }

  test("insert") {
    assert(ten.insert(-1).headOption.get === -1)
    assert(ten.insert(-1).tailOption.get.toList === ten.toList)
  }

  test("wonky time") {
    def successorsMultiplying(i: Int): SortedStream[Int] = {
      val immediateSuccessors = SortedStream.fromList(List(i + 1, i + 2))
      SortedStream.SimpleStream {
        Some(i, immediateSuccessors.flatMap(successorsMultiplying))
      }
    }
    println(successorsMultiplying(0).take(50).toList)
  }

  test("toStream") {
    println(nats.toStream)
  }
}