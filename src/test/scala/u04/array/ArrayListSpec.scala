package scala.u04.array

import org.scalacheck._
import org.scalacheck.Prop._
import java.util.{ArrayList, List => JList}
import scala.jdk.CollectionConverters._

/**
 * Extend to testing Java standard library classes like java.util.ArrayList or java.util.TreeSet
 */
object ArrayListSpec extends Properties("ArrayList"):
  val generateList: Gen[List[String]] = Gen.listOf(Gen.alphaStr)
  /** Preserve Insertion Order
   * Every time a new element is pushed inside the ArrayList, the order is preserved.
   * add(1) = ArrayList(1)
   * add(2) = ArrayList(1, 2)
   * add(-1) = ArrayList(1, 2, -1)
   */
  property("preserves insertion order") = Prop.forAll(generateList) { xs =>
    val list = new ArrayList[String]()
    xs.foreach(list.add)

    xs == list.asScala.toList
  }

  /** Data Store Law
   * Every time an element is pushed inside the ArrayList it is preserved, no matter if it is already stored.
   * add(1) = ArrayList(1)
   * add(2) = ArrayList(1, 2)
   * add(-1) = ArrayList(1, 2, -1)
   * add(1) = ArrayList(1, 2, -1, 1)
   */
  property("size matches number of inserted elements") = Prop.forAll(generateList) { xs =>
    val list = new ArrayList[String]()
    xs.foreach(list.add)

    list.size() == xs.size
  }

  /** Contains Law
   * Every time we check if the ArrayList contains an existing value, it must return true
   * add(1) = ArrayList(1)
   * add(2) = ArrayList(1, 2)
   * add(-1) = ArrayList(1, 2, -1)
   * -1 in ArrayList ? == True
   */
  property("contains returns true for added elements") = Prop.forAll(generateList) { xs =>
    val list = new ArrayList[String]()
    xs.foreach(list.add)

    xs.forall(list.contains)
  }

  /** Shrinking Law
   * Every time we remove an element that is stored inside the ArrayList, it shrinks its size by one.
   * add(1) = ArrayList(1)
   * add(2) = ArrayList(1, 2)
   * add(-1) = ArrayList(1, 2, -1)
   * remove(1, ArrayList) = ArrayList(2, -1)
   */
  property("removal reduces size by one") = Prop.forAll(generateList) { xs =>
    val list = new ArrayList[String]()
    xs.foreach(list.add)

    val sizeBefore = list.size()
    val removed = list.remove(xs.head) // removes first occurrence

    removed && list.size() == sizeBefore - 1
  }
