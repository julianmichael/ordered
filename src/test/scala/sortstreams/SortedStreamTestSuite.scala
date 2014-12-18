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

  test("toStream") {
    assert(nats.toStream.toString === "Stream(0, ?)")
  }

  test("remove") {
    assert(nats.remove(0).headOption.get === 1)
  }

  test("filter") {
    assert(nats.filter(_ % 2 == 0).take(3).toList === List(0, 2, 4))
  }

  test("takeWhile") {
    assert(nats.takeWhile(_ < 10).toList === nats.take(10).toList)
  }

  test("takeFirst") {
    assert(nats.takeFirst.toList === List(0))
    assert((nats merge nats).takeFirst.toList === List(0, 0))
  }
}