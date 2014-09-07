package sortstreams

/*
 * Sorted streams are interesting. There is a nice, lazy, easily computable way
 * to map and flatten them. But the monad structure is not as simple as that.
 * Indeed, we have a couple of extra requirements:
 *
 *  - For any type A, a SortedStream[A] also knows an ordering on A, and
 *
 *  - Mapping a SortedStream[A] over f: A => B requires that f is monotonic
 *  increasing with respect to known orders on A and B.
 *
 *  The monotonicity requirement on `f` makes it seem as though the orderings
 *  are kind of "inexorably" tied to the types, and indeed this is the case. For
 *  unfortunately, SortedStream is NOT a monad over the category of Scala types.
 *
 *  It is actually a Monad over the category of *ordered* scala types: different
 *  orderings mean different types. Or, an "ordered type" is a pair of a type A
 *  and an object ord of type Ordering[A]. A morphism in this category must
 *  preserve the orderings in its domain and codomain, which simply means it
 *  must be monotonic (increasing). In this category, SortedStream is a
 *  perfectly normal monad. But since we don't have dependent types,
 *  SortedStream must instead carry around all of these dangerous and annoying
 *  implicits and unchecked requirements.
 */

sealed trait SortedStream[A] {

  implicit val ord: Ordering[A]
  private[this] val self = this
  import SortedStream._

  def uncons: Option[(A, SortedStream[A])]

  def headOption: Option[A] = uncons map (_._1)
  def tailOption: Option[SortedStream[A]] = uncons map (_._2)

  def insert(el: A): SortedStream[A] = SimpleStream {
    self.uncons match {
      case None => Some((el, empty[A]))
      case Some((head, tail)) =>
        if(ord.lteq(el, head)) Some(el, self)
        else Some(head, tail.insert(el))
    }
  }

  // additional requirement: f must be MONOTONIC INCREASING!!!!
  def map[B](f: A => B)(implicit o: Ordering[B]): SortedStream[B] = SimpleStream {
    self.uncons flatMap {
      case (head, tail) => tail.uncons match {
        case None => Some((f(head), empty[B]))
        case Some((second, _)) =>
          if(self.ord.lt(head, second)) Some((f(head), tail.map(f)))
          else tail.map(f).insert(f(head)).uncons
      }
    }
  }

  // monadic join
  def flatten[B](
      implicit ev: A =:= SortedStream[B], ev2: SortedStream[B] =:= A,
      o: Ordering[B]): SortedStream[B] = SimpleStream {
    self.uncons flatMap {
      case (headStream, tailStreams) => headStream.uncons match {
        case None => tailStreams.flatten.uncons
        case Some((headEl, tail)) =>
          Some((headEl, tailStreams.insert(tail).flatten))
      }
    }
  }

  // and our wonderful friend flatMap, yay!!!!! (requirement is the same as for
  // map: must be monotonic)
  def flatMap[B](f: A => SortedStream[B])(implicit ordB: Ordering[B]): SortedStream[B] = {
    implicit val streamOrder = streamOrdering(ordB)
    map(f).flatten
  }

  def filter(p: A => Boolean): SortedStream[A] = SimpleStream {
    self.uncons match {
      case None => None
      case Some((head, tail)) =>
        if(p(head)) Some((head, tail.filter(p)))
        else tail.filter(p).uncons
    }
  }

  // take two sorted streams and put them into one. special case of flatten
  def merge(other: SortedStream[A]): SortedStream[A] = SimpleStream {
    (self.uncons, other.uncons) match {
      case (None, None) => None
      case (x, None) => x
      case (None, x) => x
      case (Some((head1, tail1)), Some((head2, tail2))) =>
        if (ord.lteq(head1, head2)) Some((head1, tail1.merge(other)))
        else Some((head2, tail2.merge(self)))
    }
  }

  def take(n: Int): SortedStream[A] =
    if(n <= 0) empty[A]
    else SimpleStream {
      self.uncons map {
        case (head, tail) => (head, tail.take(n-1))
      }
    }

  def takeWhile(f: A => Boolean): SortedStream[A] = SimpleStream {
    self.uncons flatMap {
      case (head, tail) =>
        if(f(head)) Some((head, tail.takeWhile(f)))
        else None
    }
  }

  def remove(a: A): SortedStream[A] = SimpleStream {
    self.uncons flatMap {
      case (head, tail) =>
        if(a == head) tail.uncons
        else Some((head, tail.remove(a)))
    }
  }

  // careful to take(n) first! this won't terminate if you're infinite
  def toList: List[A] = uncons.toList flatMap {
    case (head, tail) => head :: tail.toList
  }

  def toStream: Stream[A] = self.uncons match {
    case None => Stream.empty
    case Some((head, tail)) => head #:: tail.toStream
  }
}

object SortedStream extends SortedStreamInstances {
  object SimpleStream {
    def apply[A](
        unconsBlock: => Option[(A, SortedStream[A])])(
        implicit o: Ordering[A]): SortedStream[A] = new SortedStream[A] {
      val ord = o
      lazy val uncons = unconsBlock
    }
  }
  def empty[A](implicit o: Ordering[A]): SortedStream[A] = new SortedStream[A] {
    val ord = o
    val uncons = None
  }
  def unit[A](el: A)(implicit o: Ordering[A]): SortedStream[A] = new SortedStream[A] {
    val ord = o
    val uncons = Some(el, empty)
  }
  // requirement: for all a: A, a <= s(a)
  def recurrence[A](z: A, s: A => A)(implicit o: Ordering[A]): SortedStream[A] = new SortedStream[A] {
    val ord = o
    lazy val uncons = Some((z, recurrence(s(z), s)))
  }

  def fromList[A](list: List[A])(implicit ord: Ordering[A]): SortedStream[A] = {
    SimpleStream {
      list.sorted match {
        case Nil => None
        case head :: tail => Some(head, fromList(tail))
      }
    }
  }

  def fromOption[A](opt: Option[A])(implicit ord: Ordering[A]): SortedStream[A] = opt match {
    case None => SortedStream.empty
    case Some(x) => SortedStream.unit(x)
  }
}

trait SortedStreamInstances {
  def streamOrdering[A](ord: Ordering[A]): Ordering[SortedStream[A]] =
      new Ordering[SortedStream[A]] {
    def compare(a: SortedStream[A], b: SortedStream[A]): Int = (a.uncons, b.uncons) match {
      case (None, None) => 0
      case (None, _) => -1
      case (_, None) => 1
      case (Some((x, _)), Some((y, _))) => ord.compare(x, y)
    }
  }
}

object SortedStreamExamples {
  def intsFrom(x: Int) = SortedStream.recurrence(x, ((y: Int) => y + 1))
}