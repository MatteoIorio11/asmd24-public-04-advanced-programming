package scala.u04.treeset
import org.scalacheck.Prop.{forAll, someFailing}
import org.scalacheck.{Gen, Prop, Properties}

import java.util
import java.util.TreeSet
import scala.collection.mutable

/**
 * Extend to testing Java standard library classes like java.util.ArrayList or java.util.TreeSet
 */
object TreeSetSpec extends Properties("TreeSet"):
  val generateContainer: Gen[List[Int]] = Gen.containerOf[List, Int](Gen.choose(-1000, 1000))
  val generateNonEmptyContainer: Gen[List[Int]] = Gen.nonEmptyContainerOf[List, Int](Gen.choose(-1000, 1000))
  val generateList: Gen[List[Int]] = Gen.listOf(Gen.choose(0, 100))
  val generateOrderedList: Gen[List[Int]] = Gen.const(List(5, 4, 3, 2, 1))
  /** Ordered law
   * Every time a new element is pushed inside the TreeSet, the structure is always sorted.
   * order(add(1)) = TreeSet(1)
   * order(add(1), add(2)) = TreeSet(1, 2)
   * order(add(1), add(2), add(-1)) = TreeSet(-1, 1, 2)
   */
  property("is sorted") = forAll(generateContainer) { xs =>
    val treeSet = new util.TreeSet[Int]()
    xs.foreach(treeSet.add)

    val scalaList = treeSet.stream().toList
    val sortedList = scalaList.stream().sorted().toList
    scalaList == sortedList
  }

  /** No Duplicates Law
   * The TreeSet data structure does not contain duplicates.
   * add(1) = TreeSet(1)
   * add(2) = TreeSet(1, 2)
   * add(1) = TreeSet(1, 2)
   */
  property("no duplicates (set semantics)") = forAll(generateList) { xs =>
    val treeSet = new util.TreeSet[Int]()
    xs.foreach(treeSet.add)

    treeSet.size() == xs.distinct.size
  }

  /** Insertion Law
   * The TreeSet stores all the inserted elements.
   * add(1) = TreeSet(1)
   * add(2) = TreeSet(1, 2)
   */
  property("contains all inserted elements") = forAll(generateList) { xs =>
    val treeSet = new util.TreeSet[Int]()
    xs.foreach(treeSet.add)

    xs.forall(treeSet.contains)
  }

  /** Order of Data Law
   * Because all the elements are sorted, the first element is lower than the last element stored.
   * add(1) = TreeSet(1)
   * add(2) = TreeSet(1, 2)
   * add(3) = TreeSet(1, 2, 3)
   * first(tree) <= last(tree)
   */
  property("head <= tail for non-empty set") = forAll(generateNonEmptyContainer) { xs =>
    val treeSet = new util.TreeSet[Int]()
    xs.foreach(treeSet.add)
    treeSet.first() <= treeSet.last()
  }

  /** Empty law
   * isEmpty(add(1)) = false
   * isEmpty(empty()) = true
   */
  property("empty law") = forAll(generateNonEmptyContainer) { xs =>
    val treeSet = new util.TreeSet[Int]()
    xs.foreach(treeSet.add)

    !treeSet.isEmpty
  } && forAll(generateList) { xs =>

    val treeSet = new util.TreeSet[Int]()
    treeSet.isEmpty
  }

  /** First is always minimum value law
   * The first value inside the TreeSet is always the minimum value stored.
   * first(add(1)) = 1
   * first(add(2), add(1)) = 1
   */
  property("first minimum law") = forAll(generateOrderedList) { list =>
    val treeSet = new util.TreeSet[Int]()
    list.forall(currentMin => {
      treeSet.add(currentMin)
      treeSet.first() == currentMin
    })
  }

  /** Last is always maximum value law
   * The last value inside the TreeSet is always the maximum value stored.
   * last(add(1)) = 1
   * last(add(1, 2)) = 2
   * last(add(3, 1, 2)) = 3
   */
  property("todo") = forAll(generateOrderedList) { list =>
    val treeSet = new util.TreeSet[Int]()
    val maxValue = list.head
    list.forall(currentMin => {
      treeSet.add(currentMin)
      treeSet.last() == maxValue
    })
  }

