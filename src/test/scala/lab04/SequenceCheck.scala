package scala.lab04

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}

import scala.lab04.Sequences.*
import scala.lab04.Sequences.Sequence.*
import org.scalacheck.Prop.*

import scala.annotation.tailrec


object SequenceCheck extends Properties("Sequence"):

  // define a recursive generator of lists, monadically
  def sequenceGen[A: Arbitrary](): Gen[Sequence[A]] = for
    i <- arbitrary[A]
    b <- Gen.prob(0.8)
    s <- if b then sequenceGen().map(s2 => Cons(i, s2)) else Gen.const(Nil())
  yield s

  def generateSequence[X](size: Int, generator: Gen[X]): Sequence[X] =
    @tailrec
    def generateRecursiveSequence(currentSize: Int, currentSequence: Sequence[X]): Sequence[X] =
      if (currentSize >= size) then
        return currentSequence
      generateRecursiveSequence(currentSize + 1, Cons(generator.sample.get, currentSequence))
    generateRecursiveSequence(0, Nil())


  val genSequenceSize: Gen[Int] = Gen.choose(10, 100)
  val genOnes: Gen[Int] = Gen.const(1)
  val genRandomValue: Gen[Int] = Gen.choose(1, 10)

  // define custom arbitrary lists and mappers
  given intSeqArbitrary: Arbitrary[Sequence[Int]] = Arbitrary(sequenceGen[Int]())
  given mapperArbitrary: Arbitrary[Int => Int] = Arbitrary(Gen.oneOf[Int => Int]( _+1, _*2, x => x*x))

  // check axioms, universally
  property("mapAxioms") =
    forAll: (seq: Sequence[Int], f: Int => Int) =>
      //println(seq); println(f(10)) // inspect what's using
      (seq, f) match
        case (Nil(), f) =>  map(Nil())(f) == Nil()
        case (Cons(h, t), f) => map(Cons(h, t))(f) == Cons(f(h), map(t)(f))

  property("(sum): the total sum of a sequence of all 1 should be equal to its size") =
    forAll(genSequenceSize) {(sequenceSize) =>
      val sequence: Sequence[Int] = generateSequence(sequenceSize, genOnes)
      sequence.sum == sequenceSize
    }
    

  property("(filter): filtering a list with a predicate value > 1 with a sequence of all ones should return an empty sequence") =
    forAll(genSequenceSize) {(size) =>
      val sequence = generateSequence(size, genOnes)
      sequence.filter(value => value > 1).sum == 0
    }


  // how to check a generator works as expected
  @main def showSequences() =
    Range(0,20).foreach(i => println(summon[Arbitrary[Sequence[Int]]].arbitrary.sample))
