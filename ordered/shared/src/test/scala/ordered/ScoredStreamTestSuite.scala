package ordered

import utest._

object ScoredStreamTestSuite extends TestSuite {
  import ScoredStreamExamples._
  val nats = intsFrom(0)
  val ten = nats.take(10)

  def time(compute: => Unit): Long = {
    val begin = System.nanoTime
    compute
    val end = System.nanoTime
    end - begin
  }

  // TODO need way more tests lol

  val tests = this {
    "ten first natural numbers" - {
      assert(ten.toList == List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
    }

    "insert" - {
      assert(ten.insert(Scored(-1, 0.0)).headOption.get.item == -1)
      assert(ten.insert(Scored(-1, 0.0)).tailOption.get.toList == ten.toList)
    }

    "toStream" - {
      assert(nats.toStream.toString == "Stream(0, ?)")
    }

    "remove" - {
      assert(nats.removeFirst(_ == 0).headOption.get.item == 1)
    }

    "mapScored" - {
      val multiplesOf4 = nats.mapScored {
        case i => Scored(4 * i, i)
      }.take(4).toList
      assert(multiplesOf4 == List(0, 4, 8, 12))
    }

    "flatMap" - {
      val grid = nats.flatMap(intsFrom)
      assert(grid.take(15).toList == List(0, 1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4))

      val bigly = nats.flatMap {
        case i => ScoredStream.unit(Scored(i, i))
      }
    }

    "filter" - {
      assert(nats.filter(_ % 2 == 0).take(3).toList == List(0, 2, 4))
    }

    "takeWhile" - {
      assert(nats.takeWhile(_ < 10).toList == nats.take(10).toList)
    }

    "collect" - {
      val multiplesOf4 = nats.collect {
        case i if i % 2 == 0 => Scored(2 * i, i)
      }.take(4).toList
      assert(multiplesOf4 == List(0, 4, 8, 12))
    }

    // "merge" - {

    // }

    "fromIndexedSeq" - {
      val someVector = Vector.fill(7000)(util.Random.nextInt(200))
      val someVectorScored = someVector.map(x => Scored(x, x.toDouble))

      // // time stuff
      // val oldTime = time {
      //   println(OrderedStream.fromSortedSeq(someVector.sorted).headOption)
      // }
      // println(oldTime / 1000)

      // val newTime = time {
      //   println(OrderedStream.fromIndexedSeq(someVector).headOption)
      // }
      // println(newTime / 1000)

      assert(ScoredStream.fromIndexedSeq(someVectorScored).toList.toVector == someVector.sorted)
    }

    // XXX wtf, why does this cause the Scala compiler to hang......
    // "collect" - {
    //   val v = Vector.fill(500)(util.Random.nextInt(30))
    //   val init = OrderedStream.empty[Int].insert(4).insert(5)
    //   val ok = OrderedStream.fromIndexedSeq[Int](v): OrderedStream[Int]
    //   val orderedThing = ok.collect[Int] {
    //     case x if x % 2 == 0 => 2 * x
    //   }.toList.toVector
    //   // val roundTrip = OrderedStream.fromIndexedSeq(v)//.collect {
    //   //   case i if i % 2 == 0 => 2 * i
    //   // }.toList.toVector
    //   val reference = v.filter(_ % 2 == 0).map(_ * 2).sorted
    //   assert(v == v)
    // }

    // "collectMonotone" - {
    //   val v = Vector.fill(500)(util.Random.nextInt(30))
    //   val roundTrip = OrderedStream.fromIndexedSeq(v).collectMonotone {
    //     case i if i % 2 == 0 => 2 * i
    //   }.toList.toVector
    //   val reference = v.filter(_ % 2 == 0).map(_ * 2).sorted
    //   assert(roundTrip == reference)
    // }

    // "lazy unapply" - {
    //   var x = false
    //   val stream = OrderedStream.unit(0).
    //   assert(nats.takeWhile(_ < 10).toList == nats.take(10).toList)
    // }
  }
}
