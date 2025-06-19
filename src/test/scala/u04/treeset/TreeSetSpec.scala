package scala.u04.treeset
import java.util
import java.util.TreeSet

/**
 * Extend to testing Java standard library classes like java.util.ArrayList or java.util.TreeSet
 */
object TreeSetSpec extends Properties("TreeSet") {

  property("is sorted") = Prop.forAll(Gen.containerOf[List, Int](Gen.choose(-1000, 1000))) { xs =>
    val treeSet = new util.TreeSet[Int]()
    xs.foreach(treeSet.add)

    val scalaList = treeSet.asScala.toList
    scalaList == scalaList.sorted
  }

  property("no duplicates (set semantics)") = Prop.forAll(Gen.listOf(Gen.choose(0, 100))) { xs =>
    val treeSet = new util.TreeSet[Int]()
    xs.foreach(treeSet.add)

    treeSet.size() == xs.distinct.size
  }

  property("contains all inserted elements") = Prop.forAll(Gen.listOf(Gen.choose(0, 100))) { xs =>
    val treeSet = new util.TreeSet[Int]()
    xs.foreach(treeSet.add)

    xs.forall(treeSet.contains)
  }

  property("head <= tail for non-empty set") = Prop.forAll(Gen.nonEmptyContainerOf[List, Int](Gen.choose(-1000, 1000))) { xs =>
    val treeSet = new util.TreeSet[Int]()
    xs.foreach(treeSet.add)

    val sorted = treeSet.asScala.toList
    sorted.head <= sorted.last
  }
}

