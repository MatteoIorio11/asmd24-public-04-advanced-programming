package scala.u04.array

import org.scalacheck.*
import org.scalacheck.Prop.*

import java.util
import java.util.{ArrayList, List as JList}
import scala.jdk.CollectionConverters.*

/**
 * Extend to testing Java standard library classes like java.util.ArrayList or java.util.TreeSet
 */
object ArrayListSpec extends Properties("ArrayList"):
  val generateList: Gen[List[String]] = Gen.nonEmptyListOf(Gen.alphaStr)
  val generateValue: Gen[String] = Gen.alphaStr
  /** Preserve Insertion Order
   * Every time a new element is pushed inside the ArrayList, the order is preserved.
   * add(1) = ArrayList(1)
   * add(2) = ArrayList(1, 2)
   * add(-1) = ArrayList(1, 2, -1)
   */
  property("preserves insertion order") = forAll(generateList) { xs =>
    val list = new util.ArrayList[String]()
    xs.foreach(list.add)

    xs == list.asScala.toList
  }

  /** Insertion Law
   * head(add(a)) = a
   */
  property("insertion law") = forAll(generateValue) { str =>
    val list = new util.ArrayList[String]()
    list.add(str)
    str == list.get(0)
  }

  /** Empty law
   * isEmpty(add(a)) = false
   * isEmpty(empty()) = true
   */
  property("empty law") = forAll(generateValue) { str =>
    val list = new util.ArrayList[String]()
    list.add(str)
    !list.isEmpty
  } && forAll(generateValue){ str =>
    val list = new util.ArrayList[String]()
    list.isEmpty
  }


  /** Data Store Law
   * Every time an element is pushed inside the ArrayList it is preserved, no matter if it is already stored.
   * add(1) = ArrayList(1)
   * add(2) = ArrayList(1, 2)
   * add(-1) = ArrayList(1, 2, -1)
   * add(1) = ArrayList(1, 2, -1, 1)
   */
  property("size matches number of inserted elements") = forAll(generateList) { xs =>
    val list = new util.ArrayList[String]()
    xs.foreach(list.add)

    list.size() == xs.size
  }

  /** Contains Law
   * Every time we check if the ArrayList contains an existing value, it must return true
   * add(1) = ArrayList(1)
   * add(2) = ArrayList(1, 2)
   * add(-1) = ArrayList(1, 2, -1)
   * contains(-1) == True
   */
  property("contains returns true for added elements") = forAll(generateList) { xs =>
    val list = new util.ArrayList[String]()
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
  property("removal reduces size by one") = forAll(generateList) { xs =>
    val list = new util.ArrayList[String]()
    xs.foreach(list.add)

    val sizeBefore = list.size()
    val removed = list.remove(xs.head) // removes first occurrence

    removed && list.size() == sizeBefore - 1
  }

  /** Tail law
   * The order of insertion is always preserved, the last add element will be in the last position.
   * add(a)
   * tail(add(b)) = b
   */
  property("tail law") = forAll(generateValue, generateValue) { (head, tail) =>
    val list = new util.ArrayList[String]()
    list.add(head)
    list.add(tail)
    tail == list.get(list.size() - 1)
  }
