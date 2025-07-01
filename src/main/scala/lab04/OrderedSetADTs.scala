package scala.lab04

import u04.datastructures.Sequences
import u04.datastructures.Sequences.Sequence
import u04.datastructures.Sequences.Sequence.{Cons, Nil}

import scala.annotation.tailrec
import scala.collection.immutable.TreeSet
import scala.lab04.SetADTs.SetADT

object OrderedSetADTs:
  trait OrderedSetADT[A: Ordering] extends SetADT[A]

  class TreeSetADT[A: Ordering] extends OrderedSetADT[A]:
    opaque type Set = TreeSet[A]

    override def empty(): TreeSet[A] = TreeSet()

    extension (s: TreeSet[A])
      override def add(element: A): TreeSet[A] = s ++ TreeSet(element)
      override def contains(a: A): Boolean = s.union(TreeSet(a)) === s
      override def union(other: TreeSet[A]): TreeSet[A] = s | other
      override def intersection(other: TreeSet[A]): TreeSet[A] = s & other
      override def remove(a: A): TreeSet[A] = s &~ TreeSet(a)
      override def toSequence(): Sequences.Sequence[A] =
        val list = s.toList
        val size: Int = list.size
        def buildSequence(index: Int, sequenceSize: Int): Sequence[A] = index match
          case x if x == sequenceSize => Nil()
          case _ => Cons(list(index), buildSequence(index + 1, size))
        buildSequence(0, size)
      override def size(): Int = s.toList.size
      override def ===(other: TreeSet[A]): Boolean = s.equals(other)

