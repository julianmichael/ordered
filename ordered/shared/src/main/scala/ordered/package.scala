package object ordered {

  // requires that a is less than all elements.
  implicit class OrderedStreamConsInfixConstructors[A : Ordering](a: A) {
    @inline def :<(os: => OrderedStream[A]) = new :<(a, os)
    @inline def :<+(os: OrderedStream[A]) = new :<(a, os)
  }

  implicit class ScoredStreamConsInfixConstructors[A](a: Scored[A]) {
    @inline def ::<(ss: => ScoredStream[A]) = new ScoredCons(a, ss)
    @inline def ::<+(ss: ScoredStream[A]) = new ScoredCons(a, ss)
  }

  // maybe you could do some stuff with fancy evidence... probably no need though

  // class Monotonic[A, B, FType <: Singleton <: (A => B),
  //                 OrdAType <: Singleton <: Order[A],
  //                 OrdBType <: Singleton <: Order[A]] private (
  //   f: FType,
  //   ordA: OrdAType[A],
  //   ordB: OrdBType[B])

  // class MonotonicFunction[A, B](function: A => B)(
  //   implicit ordA: ...,
  //   ordB: ...,
  //   val mono: Monotonic[...])

  // implicit def getMonotonicityProof(implicit f: MonotonicFunction[...]) =
  //   f.mono

  // implicit def getOriginalFunction(f: MonotonicFunction[...]): A => B =
  //   f.function
}
