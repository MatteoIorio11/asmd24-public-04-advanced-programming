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


  def getSequenceGenerator[X](generator: Gen[X]): Gen[Sequence[X]] =
    for {
      size <- genSequenceSize
    } yield generateSequence(size, generator)


  val genSequenceSize: Gen[Int] = Gen.choose(10, 100)
  val genOnes: Gen[Int] = Gen.const(1)
  val genRandomValue: Gen[Int] = Gen.choose(1, 10)
  val genSequenceOfOnes: Gen[Sequence[Int]] = getSequenceGenerator(genOnes)
  val genRandomSequence: Gen[Sequence[Int]] = getSequenceGenerator(genRandomValue)

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

  property("(sum): The total sum of a sequence of all 1 should be equal to its size") =
    forAll(genSequenceSize) {(sequenceSize) =>
      val sequence: Sequence[Int] = generateSequence(sequenceSize, genOnes)
      sequence.sum == sequenceSize
    }

  property("(sum): The total sum of all the elements that are in the range [1, 10] should be greather than the total sum of the sequence.") =
    forAll(genRandomSequence) {(sequence) =>
      var size = 0
      sequence.filter(value => {
        size += 1
        true
      })
      sequence.sum >= size
    }

  property("(filter): filtering a list with a predicate value > 1 with a sequence of all ones should return an empty sequence") =
    forAll(genSequenceSize) {(size) =>
      val sequence = generateSequence(size, genOnes)
      sequence.filter(value => value > 1).sum == 0
    }

  property("(filter): The sequence of values between [1, 10] should be empty if we filter for only values equals to 0") =
    forAll(genRandomSequence) {(sequence) =>
      sequence.filter(value => value == 0).sum == 0
    }

  property("(flatMap): The generated sequence of random between [1, 10] after a flatMap that convertes every val in a Sequence of zeros should have a sum equal to 0") =
    forAll(genRandomSequence) {(sequence) =>
      sequence.flatMap(value => Cons(0, Nil())).sum == 0
    }

  // how to check a generator works as expected
  @main def showSequences() =
    Range(0,20).foreach(i => println(summon[Arbitrary[Sequence[Int]]].arbitrary.sample))
