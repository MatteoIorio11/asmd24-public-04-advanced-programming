package scala.lab04

import u04.datastructures.Sequences

import scala.collection.immutable.TreeSet
import scala.lab04.Sequences.Sequence
import scala.lab04.SetADTs.SetADT

object OrderedSetADTs:
  trait OrderedSetADT extends SetADT:
    type Set[A]
    def empty[A](): Set[A]
    extension [A: Ordering](s: Set[A])
      def add(element: A): Set[A]
      def contains(a: A): Boolean
      def union(other: Set[A]): Set[A]
      def intersection(other: Set[A]): Set[A]
      infix def ||(other: Set[A]): Set[A] = s.union(other)
      infix def &&(other: Set[A]): Set[A] = s.intersection(other)
      def remove(a: A): Set[A]
      def toSequence(): Sequence[A]
      def size(): Int
      def ===(other: Set[A]): Boolean
      override def ===(other: Set[A]): Boolean

  object TreeSetADT extends OrderedSetADT:
    opaque type Set[A] = TreeSet[A]
    def empty[A: Ordering](): Set[A] = TreeSet()

    extension [A: Ordering](s: Set[A])
      def ===(other: Set[A]): Boolean =
        s.equals(other)
      override def add(element: A): TreeSet[A] =
        s + element
      override def contains(a: A): Boolean =
        s.
      override def union(other: TreeSet[A]): TreeSet[A] = ???
      override def intersection(other: TreeSet[A]): TreeSet[A] = ???
      override def remove(a: A): TreeSet[A] = ???
      override def toSequence(): Sequences.Sequence[A] = ???
      override def size(): Int = ???
      override def ===(other: TreeSet[A]): Boolean = ???

