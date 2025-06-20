package scala.lab04

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}

import scala.lab04.BasicSetADTCheck.setADT
import scala.lab04.OrderedSetADTs.{OrderedSetADT, TreeSetADT}
import scala.lab04.SetADTs.{BasicSetADT, SetADT}

abstract class SetADTCheck(name: String) extends Properties(name):
  val setADT: SetADT[Int]
  import setADT.*

  // generating a small Int
  def smallInt(): Gen[Int] = Gen.choose(0, 10)
  // generating a Set of Int with approximate size (modulo clashes)
  def setGen[A: Arbitrary](size: Int): Gen[Set] =
    if size == 0
      then Gen.const(empty())
    else for
      a <- Arbitrary.arbitrary[Int]
      s <- setGen(size - 1)
    yield s.add(a)
  // a given instance to generate sets with small size
  given arb: Arbitrary[Set] = Arbitrary:
    for
      i <- smallInt()
      s <- setGen[Int](i)
    yield s

  /**
    * axioms defining contains based on empty/add:
    * contains(empty, x) = false
    * contains(add(x,s), y) = (x == y) || contains(s, y)
  */

  property("axioms for contains") =
     forAll: (s: Set, x: Int, y:Int) =>
        s.add(x).contains(y) == (x == y) || s.contains(y)
   &&
     forAll: (x: Int) =>
        !empty().contains(x)

  /** axioms defining union:
  * union(empty, s) == s
  * union(add(x, s2), s) = add(x, union(s2, s)
  */
  property("axioms for union") =
    forAll: (s: Set) =>
      s.union(empty()) == s
    &&
      forAll: (s1: Set, s2: Set, x: Int) =>
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
      forAll: (s1: Set, x: Int) =>
        s1.remove(x) === s1.add(x).remove(x)
    &&
      forAll: (s1: Set, x: Int, y: Int) =>
        if (x == y) true else s1.remove(x).add(y) === s1.add(y).remove(x)

  //  <<ADT-VERIFIER>>
  /** Commutative of Union
   * A ∪ B = B ∪ A
   */
  property("commutative of intersection") =
    forAll: (s1: Set, s2: Set) =>
      s1.intersection(s2) === s2.intersection(s1)


  /** Commutative of Intersection
   *  A ∩ B = B ∩ A
   */
  property("commutativity of union") =
    forAll: (s1: Set, s2: Set) =>
      (s1 || s2) === (s2 || s1)

  /** Associative property
   * (A ∪ B) ∪ C = A ∪ (B ∪ C) and (A ∩ B) ∩ C = A ∩ (B ∩ C)
   */
  property("associative property") =
    forAll: (s1: Set, s2: Set, s3: Set) =>
      (s1.union(s2)).union(s3) === s1.union(s2.union(s3))
    &&
      forAll: (s1: Set, s2: Set, s3: Set) =>
        (s1.intersection(s2)).intersection(s3) === s1.intersection(s2.intersection(s3))

  /** Idempotent property
   * (A ∪ A) = A && (A ∩ A) = A
   */
  property("idempotent") =
    forAll: (s1: Set) =>
      s1.union(s1) === s1
    &&
      forAll: (s1: Set) =>
        s1.intersection(s1) === s1

  /** Distribute Law property
   * A ∩ (B ∪ C)=(A ∩ B) ∪ (A ∩ C) "intersection distributes over union"
   * A ∪ (B ∩ C) = (A ∪ B) ∩ (A ∪ C) "Union distributes over intersection"
   */
  property("distribute law") =
    forAll: (s1: Set, s2: Set, s3: Set) =>
      s1.intersection(s2.union(s3)) === s1.intersection(s2).union(s1.intersection(s3))
    &&
      forAll: (s1: Set, s2: Set, s3: Set) =>
        s1.union(s2.intersection(s3)) === s1.union(s2).intersection(s1.union(s3))

  /** Absorption Law property
   * A ∪ (A ∩ B) = A
   * A ∩ (A ∪ B) = A
   */
  property("absorption law") =
    forAll: (s1: Set, s2: Set) =>
      s1.union(s1.intersection(s2)) === s1
    &&
      forAll: (s1: Set, s2: Set) =>
        s1.intersection(s1.union(s2)) === s1

  /** Identity Law
   * A U 0 == A
   * A ∩ 0 == 0
   */
  property("identity law") =
    forAll: (s1: Set) =>
      s1.union(empty()) === s1
    &&
      forAll: (s1: Set) =>
        s1.intersection(empty()) === empty()

object BasicSetADTCheck extends SetADTCheck("SequenceBased Set"):
  val setADT: SetADT[Int]= BasicSetADT[Int]()
  @main def visuallingCheckArbitrarySets =
    Range(0, 20).foreach(i => println(summon[Arbitrary[setADT.Set]].arbitrary.sample))


object TreeSetADTCheck extends SetADTCheck("TreeSetBased Set"):
  val setADT: OrderedSetADT[Int] = TreeSetADT[Int]()
