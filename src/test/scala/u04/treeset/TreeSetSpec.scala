package scala.u04.treeset
import org.scalacheck.{Gen, Prop, Properties}

import java.util
import java.util.TreeSet

/**
 * Extend to testing Java standard library classes like java.util.ArrayList or java.util.TreeSet
 */
object TreeSetSpec extends Properties("TreeSet"):
  /** Ordered law
   * Every time a new element is pushed inside the TreeSet, the structure is always sorted.
   * add(1) = TreeSet(1)
   * add(2) = TreeSet(1, 2)
   * add(-1) = TreeSet(-1, 1, 2)
   */
  property("is sorted") = Prop.forAll(Gen.containerOf[List, Int](Gen.choose(-1000, 1000))) { xs =>
    val treeSet = new util.TreeSet[Int]()
    xs.foreach(treeSet.add)

    val scalaList = treeSet.stream().toList
    scalaList == scalaList.stream().sorted()
  }

  /** No Duplicates Law
   * The TreeSet data structure does not contain duplicates.
   * add(1) = TreeSet(1)
   * add(2) = TreeSet(1, 2)
   * add(1) = TreeSet(1, 2)
   */
  property("no duplicates (set semantics)") = Prop.forAll(Gen.listOf(Gen.choose(0, 100))) { xs =>
    val treeSet = new util.TreeSet[Int]()
    xs.foreach(treeSet.add)

    treeSet.size() == xs.distinct.size
  }

  /** Insertion Law
   * The TreeSet stores all the inserted elements.
   * add(1) = TreeSet(1)
   * add(2) = TreeSet(1, 2)
   */
  property("contains all inserted elements") = Prop.forAll(Gen.listOf(Gen.choose(0, 100))) { xs =>
    val treeSet = new util.TreeSet[Int]()
    xs.foreach(treeSet.add)

    xs.forall(treeSet.contains)
  }

  /** Order of Data Law
   * Because all the elements are sorted, the first element is lower than the last element stored.
   * add(1) = TreeSet(1)
   * add(2) = TreeSet(1, 2)
   * add(3) = TreeSet(1, 2, 3)
   * TreeSet.first <= TreeSet.last
   */
  property("head <= tail for non-empty set") = Prop.forAll(Gen.nonEmptyContainerOf[List, Int](Gen.choose(-1000, 1000))) { xs =>
    val treeSet = new util.TreeSet[Int]()
    xs.foreach(treeSet.add)

    val sorted = treeSet.stream().toList
    sorted.get(0) <= sorted.get(sorted.size()-1)
  }

