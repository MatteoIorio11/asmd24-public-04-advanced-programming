package scala.u04.array

import org.scalacheck._
import org.scalacheck.Prop._
import java.util.{ArrayList, List => JList}
import scala.jdk.CollectionConverters._

/**
 * Extend to testing Java standard library classes like java.util.ArrayList or java.util.TreeSet
 */
object ArrayListSpec extends Properties("ArrayList") {

  property("preserves insertion order") = Prop.forAll(Gen.listOf(Gen.alphaStr)) { xs =>
    val list = new ArrayList[String]()
    xs.foreach(list.add)

    xs == list.asScala.toList
  }

  property("size matches number of inserted elements") = Prop.forAll(Gen.listOf(Gen.alphaStr)) { xs =>
    val list = new ArrayList[String]()
    xs.foreach(list.add)

    list.size() == xs.size
  }

  property("contains returns true for added elements") = Prop.forAll(Gen.listOf(Gen.alphaStr)) { xs =>
    val list = new ArrayList[String]()
    xs.foreach(list.add)

    xs.forall(list.contains)
  }

  property("removal reduces size by one") = Prop.forAll(Gen.nonEmptyListOf(Gen.alphaStr)) { xs =>
    val list = new ArrayList[String]()
    xs.foreach(list.add)

    val sizeBefore = list.size()
    val removed = list.remove(xs.head) // removes first occurrence

    removed && list.size() == sizeBefore - 1
  }
}

