# Verified Specification, ADTs, type classes, and monads
## Tasks
Here are listed all the tasks that I have managed to complete.
### ADT-VERIFIER
Task: 

Define a formal Abstract Data Type (ADT) for sets with essential operations: union, intersection, contains
Examine the current implementation in scala.lab04.SetADT and understand its structure
Complete the property-based tests in SetADTCheck by:
I Adding missing algebraic properties (commutativity, associativity, idempotence) for union and intersection (e.g., A ∪ B = B ∪ A) I Implementing cross-property tests (e.g., relationship between union and intersection) I Ensuring properties reflect the mathematical axioms of sets
Once tests are complete, implement an alternative version of SetADT using a Tree-based structure
Adapt the tests minimally to work with your new implementation
---
The source code regarding this task is located inside:
* OrderedSetADTs.scala
* SetADTs.scala
* SetADTCheck.scala

### JAVA-SCALA-CHECK
Task:

Use ScalaCheck as a property-based testing tool for Java code (leveraging Scala’s Java interoperability)
Implement the following steps:
I Create a simple immutable Java class (e.g., Point2D with x, y coordinates and methods like distanceTo, translate, rotate) I Write a comprehensive ScalaCheck test suite that verifies mathematical properties such as: Distance symmetry (i.e., a.distanceTo(b) == b.distanceTo(a)), Triangle inequality, Rotation
invariants I Extend to testing Java standard library classes like java.util.ArrayList or java.util.TreeSet
Reflect on the advantages and limitations of using ScalaCheck as a cross-language testing DSL

The source code regarding this task is located inside:
* Point2D.java
* PointCheck.scala
* TreeSetSpec.scala
* ArrayListSpec.scala
