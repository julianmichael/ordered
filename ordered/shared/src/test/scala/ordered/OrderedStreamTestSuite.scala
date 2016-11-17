package ordered

import utest._

object OrderedStreamTestSuite extends TestSuite {
  import OrderedStreamExamples._
  val nats = intsFrom(0)
  val ten = nats.take(10)

  // TODO need way more tests lol

  val tests = this {
    "ten first natural numbers" - {
      assert(ten.toList == List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
    }

    "insert" - {
      assert(ten.insert(-1).headOption.get == -1)
      assert(ten.insert(-1).tailOption.get.toList == ten.toList)
    }

    "toStream" - {
      assert(nats.toStream.toString == "Stream(0, ?)")
    }

    "remove" - {
      assert(nats.removeFirst(_ == 0).headOption.get == 1)
    }

    "filter" - {
      assert(nats.filter(_ % 2 == 0).take(3).toList == List(0, 2, 4))
    }

    "takeWhile" - {
      assert(nats.takeWhile(_ < 10).toList == nats.take(10).toList)
    }

    // "lazy unapply" - {
    //   var x = false
    //   val stream = OrderedStream.unit(0).
    //   assert(nats.takeWhile(_ < 10).toList == nats.take(10).toList)
    // }
  }
}
