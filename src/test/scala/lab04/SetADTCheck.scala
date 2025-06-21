package scala.lab04

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}

import scala.lab04.SetADTs.{BasicSetADT, SetADT}

abstract class SetADTCheck(name: String) extends Properties(name):
  val setADT: SetADT
  import setADT.*

  // generating a small Int
  def smallInt(): Gen[Int] = Gen.choose(0, 10)
  // generating a Set of Int with approximate size (modulo clashes)
  def setGen[A: Arbitrary](size: Int): Gen[Set[A]] =
    if size == 0
      then Gen.const(empty())
    else for
      a <- Arbitrary.arbitrary[A]
      s <- setGen(size - 1)
    yield s.add(a)
  // a given instance to generate sets with small size
  given arb: Arbitrary[Set[Int]] = Arbitrary:
    for
      i <- smallInt()
      s <- setGen[Int](i)
    yield s

  property("commutativity of union") =
    forAll: (s1: Set[Int], s2: Set[Int]) =>
      (s1 || s2) === (s2 || s1)

  /**
    * axioms defining contains based on empty/add:
    * contains(empty, x) = false
    * contains(add(x,s), y) = (x == y) || contains(s, y)
  */

  property("axioms for contains") =
     forAll: (s: Set[Int], x: Int, y:Int) =>
        s.add(x).contains(y) == (x == y) || s.contains(y)
   &&
     forAll: (x: Int) =>
        !empty().contains(x)

  /** axioms defining union:
  * union(empty, s) == s
  * union(add(x, s2), s) = add(x, union(s2, s)
  */
  property("axioms for union") =
    forAll: (s: Set[Int]) =>
      s.union(empty()) == s
    &&
      forAll: (s1: Set[Int], s2: Set[Int], x: Int) =>
        (s1.add(x)).union(s2) === (s2.add(x)).union(s1)

  /** axioms defining remove:
   * remove(x, empty) = empty
   * remove(x, add(x, s)) = remove(x, s)
   * remove(x, add(y, s)) = add(y, remove(x, s)) if x!=y
   */
  property("axioms for remove") =
    forAll: (x: Int) =>
       empty().remove(x) === empty()
    &&
      forAll: (s1: Set[Int], x: Int) =>
        s1.remove(x) === s1.add(x).remove(x)
    &&
      forAll: (s1: Set[Int], x: Int, y: Int) =>
        if (x == y) true else s1.remove(x).add(y) === s1.add(y).remove(x)



object BasicSetADTCheck extends SetADTCheck("SequenceBased Set"):
  val setADT: SetADT = BasicSetADT

  @main def visuallingCheckArbitrarySets =
    Range(0,20).foreach(i => println(summon[Arbitrary[setADT.Set[Int]]].arbitrary.sample))
