package scala.lab04

import u04.datastructures.Sequences.*
import Sequence.*

import scala.collection.immutable.TreeSet

object SetADTs:
  
  trait SetADT[A]:
    type Set
    def empty(): Set
    extension (s: Set)
      def add(element: A): Set
      def contains(a: A): Boolean
      def union(other: Set): Set
      def intersection(other: Set): Set
      infix def ||(other: Set): Set = s.union(other)
      infix def &&(other: Set): Set = s.intersection(other)
      def remove(a: A): Set
      def toSequence(): Sequence[A]
      def size(): Int
      def ===(other: Set): Boolean



  class BasicSetADT[A] extends SetADT[A]:

    opaque type Set = Sequence[A]

    def empty(): Set = Nil()

    extension (s: Set)
      def add(element: A): Set = s match
        case Cons(h, _) if h == element => s
        case Cons(h, t)  => Cons(h, t.add(element))
        case _ => Cons(element, Nil())

      def remove(a: A): Set = s.filter(_ != a)

      def contains(a: A): Boolean = s match
        case Cons(h, t) => h == a || t.contains(a)
        case Nil() => false

      def toSequence(): Sequence[A] = s

      def union(s2: Set): Set = s2 match
        case Cons(h, t) => Cons(h, s.remove(h).union(t))
        case Nil() => s

      def intersection(s2: Set): Set = s match
        case Cons(h, t) if s2.contains(h) => Cons(h, t.intersection(s2.remove(h)))
        case Cons(_, t) => t.intersection(s2)
        case Nil() => Nil()

      def size(): Int = s match
        case Cons(_, t) => 1 + t.size()
        case Nil() => 0

      def ===(other: Set): Boolean =
        s.union(other).size() == s.size()



@main def trySetADTModule =
  import SetADTs.*
  val setADT: SetADT[Int] = BasicSetADT()
  import setADT.*

  val s1 = empty().add(10).add(20).add(30)
  val s2 = empty().add(10).add(11)
  // val s3: Set[Int] = Cons(10, Nil()) // because Set is defined opaque
  println(s1.toSequence()) // (10, 20, 30)
  println(s2.toSequence()) // (10, 11)
  println(s1.union(s2).toSequence()) // (10, 20, 30, 11)
  println(s1.intersection(s2).toSequence()) // (10)
