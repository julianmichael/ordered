package sortstreams

/* A second shot at sorted streams.
 * This implementation is much cleaner and has slightly different properties:
 * in particular, it always evaluates until it knows its head (or is empty).
 * So if you ever dropWhile or filter out all the elements, you'll immediately go into an infinite loop.
 *
 * Also I realized there are really two kinds of monotone functions we're dealing with here;
 * they may respect the strict ordering (while possibly changing the order between "equal" things)
 * or (more strongly) the non-strict ordering, ensuring that equal things remain equal.
 * The second may be implemented more efficiently, not looking at any of the children at all,
 * whereas the first always has to look at at least one child to make sure the head is still the smallest.
 */

sealed abstract class OrderedStream[A](implicit private val order: Ordering[A]) {

  import OrderedStream._

  def headOption: Option[A]
  def tailOption: Option[OrderedStream[A]]
  def isEmpty: Boolean
  def ifNonEmpty: Option[:<[A]]

  // prefer pattern matching directly to uncons
  // def uncons: Option[(A, OrderedStream[A])]

  def insert(a: A): OrderedStream[A]

  // f must respect the strict version of the preorder, as in: a < b => f(a) < f(b).
  def map[B : Ordering](f: A => B): OrderedStream[B]
  // STRONGER requirement: f must respect the non-strict version of the preorder, as in: a <= b => f(a) <= f(b)
  // more efficient because we don't need to examine the last element.
  def mapMonotone[B : Ordering](f: A => B): OrderedStream[B]

  // monadic join
  def flatten[B : Ordering](implicit ev: A =:= OrderedStream[B], ev2: OrderedStream[B] =:= A): OrderedStream[B]

  // f again must be monotonic (strict)
  def flatMap[B : Ordering](f: A => OrderedStream[B]): OrderedStream[B] =
    map(f).flatten

  // f again must be monotonic (non-strict)
  def flatMapMonotone[B : Ordering](f: A => OrderedStream[B]): OrderedStream[B] =
    mapMonotone(f).flatten

  // be careful about filtering all of the elements from an infinite stream---that will cause nontermination
  def filter(p: A => Boolean): OrderedStream[A]

  def merge(other: OrderedStream[A]): OrderedStream[A] = other

  def take(n: Int): OrderedStream[A]

  def takeWhile(f: A => Boolean): OrderedStream[A]

  def drop(n: Int): OrderedStream[A]

  def dropWhile(f: A => Boolean): OrderedStream[A]

  def removeFirst(p: A => Boolean): OrderedStream[A]

  // careful to take(n) first! this won't terminate if you're infinite
  def toList: List[A]

  def toStream: Stream[A]
}

object OrderedStream extends OrderedStreamInstances {

  // assumes a is less than all elements: for internal use.
  implicit protected[sortstreams] class OrderedStreamConsInfixConstructor[A : Ordering](a: A) {
    @inline def :<(os: => OrderedStream[A]) = new :<(a, os)
  }

  def empty[A : Ordering]: OrderedStream[A] =
    ONil[A]

  def unit[A : Ordering](el: A): OrderedStream[A] =
    el :< ONil[A]

  // requirement: for all a: A, a <= s(a)
  def recurrence[A : Ordering](z: A, s: A => A): OrderedStream[A] =
    z :< recurrence(s(z), s)

  // TODO: more efficient "lazy quicksort" sounds like fun
  def fromIndexedSeq[A : Ordering](is: IndexedSeq[A]): OrderedStream[A] =
    is.sorted.foldRight(empty[A])(_ :< _)

  def fromSeq[A : Ordering](is: Seq[A]): OrderedStream[A] =
    fromIndexedSeq(is.toIndexedSeq)

  def fromIterator[A : Ordering](is: Iterator[A]): OrderedStream[A] =
    fromIndexedSeq(is.toVector)

  def fromOption[A : Ordering](opt: Option[A]): OrderedStream[A] = opt match {
    case None => empty[A]
    case Some(a) => unit(a)
  }
}

import OrderedStream._

class ONil[A](implicit order: Ordering[A]) extends OrderedStream[A]()(order) {
  override def headOption = None
  override def tailOption = None
  override def isEmpty = true
  override def ifNonEmpty = None
  override def insert(a: A): OrderedStream[A] = a :< this

  override def map[B : Ordering](f: A => B) =
    ONil[B]
  override def mapMonotone[B : Ordering](f: A => B) =
    ONil[B]
  override def flatten[B : Ordering](implicit ev: A =:= OrderedStream[B], ev2: OrderedStream[B] =:= A) =
    ONil[B]
  override def filter(p: A => Boolean) =
    this
  override def merge(other: OrderedStream[A]) =
    other
  override def take(n: Int) =
    this
  override def takeWhile(f: A => Boolean) =
    this
  override def drop(n: Int) =
    this
  override def dropWhile(f: A => Boolean) =
    this
  override def removeFirst(p: A => Boolean) =
    this
  override def toList = Nil
  override def toStream = Stream.empty[A]
}

object ONil {
  def apply[A : Ordering] = new ONil[A]
  def unapply(sc: ONil[_]): Boolean = true
}

// assumes head is lower order than everything in tail
class :<[A] protected[sortstreams] (
  val head: A,
  _tail: => OrderedStream[A])(implicit order: Ordering[A]) extends OrderedStream[A]()(order) {
  lazy val tail = _tail

  override def headOption = Some(head)
  override def tailOption = Some(tail)
  override def isEmpty = false
  override def ifNonEmpty = Some(this)

  override def insert(a: A): :<[A] = {
    if(order.lteq(a, head)) a :< this
    else head :< tail.insert(a)
  }

  override def map[B : Ordering](f: A => B) = mapAux(f(head), f)
  private def mapAux[B : Ordering](fhead: B, f: A => B): :<[B] = tail match {
    case ONil() => fhead :< ONil[B]
    case t @ :<(second, _) =>
      val fsecond = f(second)
      // first case needed to break the recursion
      if(implicitly[Ordering[B]].lteq(fhead, fsecond)) fhead :< t.mapAux(fsecond, f)
      else t.mapAux(fsecond, f).insert(fhead)
  }
  override def mapMonotone[B : Ordering](f: A => B) =
    f(head) :< tail.mapMonotone(f)

  override def flatten[B : Ordering](implicit ev: A =:= OrderedStream[B], ev2: OrderedStream[B] =:= A) = head match {
    case ONil() => tail.flatten[B]
    case h :< t => tail.insert(t().asInstanceOf[A]).flatten[B] // TODO this is totally safe, but why does the compiler not like it without the cast?
  }

  override def filter(p: A => Boolean) = if(p(head)) {
    head :< tail.filter(p)
  } else {
    tail.filter(p)
  }

  override def merge(other: OrderedStream[A]) = other match {
    case ONil() => this
    case h :< t => if(order.lteq(head, h)) {
      head :< tail.merge(other)
    } else {
      h :< t().merge(this)
    }
  }

  override def take(n: Int) = if(n <= 0) {
    empty[A]
  } else {
    head :< tail.take(n - 1)
  }

  override def takeWhile(p: A => Boolean) = if(p(head)) {
    head :< tail.takeWhile(p)
  } else {
    empty[A]
  }

  override def drop(n: Int) = tail.drop(n - 1)

  override def dropWhile(p: A => Boolean) = if(p(head)) {
    tail.dropWhile(p)
  } else {
    this
  }

  override def removeFirst(p: A => Boolean) = if(p(head)) {
    tail
  } else {
    head :< tail.removeFirst(p)
  }

  override def toList: List[A] = head :: tail.toList
  override def toStream: Stream[A] = head #:: tail.toStream
}

// don't evaluate the tail
object :< {
  def unapply[A](sc: :<[A]): Option[(A, () => OrderedStream[A])] = Some((sc.head, () => sc.tail))
}
// evaluate the tail
object :<+ {
  def unapply[A](sc: :<[A]): Option[(A, OrderedStream[A])] = Some((sc.head, sc.tail))
}

trait OrderedStreamInstances {
  implicit def streamOrdering[A](implicit ord: Ordering[A]): Ordering[OrderedStream[A]] = new Ordering[OrderedStream[A]] {
    def compare(a: OrderedStream[A], b: OrderedStream[A]): Int = (a.headOption, b.headOption) match {
      case (None, None) => 0
      case (None, _) => -1
      case (_, None) => 1
      case (Some(x), Some(y)) => ord.compare(x, y)
    }
  }
  implicit def consOrdering[A](implicit ord: Ordering[A]): Ordering[:<[A]] = new Ordering[:<[A]] {
    def compare(a: :<[A], b: :<[A]): Int = ord.compare(a.head, b.head)
  }
}

object OrderedStreamExamples {
  def intsFrom(x: Int) = OrderedStream.recurrence(x, ((y: Int) => y + 1))
}
