package maxel

import scalaprops._
import scalaprops.Gen._
import scalaprops.Property._
import scalaz.std.string._

object MaxelTests extends Scalaprops {
  import maxel._
  import spire.algebra.Eq
  import spire.syntax.all._
  import spire.std.char._
  import spire.std.int._
  import spire.std.boolean._
  import spire.std.option._
  import mset.Realm
  import Maxel._
  import PixelTests._

  implicit def genMaxel[A:Gen:Eq]: Gen[Maxel[A]] =
    Gen[List[Pixel[A]]].map(fromSeq)

  val extentFunctorality = forAll { m: Maxel[Int] =>
    m.transpose.extent === m.extent.map(_.transpose)
  }

  val intentFunctorality = forAll { m: Maxel[Int] =>
    m.transpose.intent === m.intent.map(_.transpose)
  }

  val constructiveDiagonality = forAll { (m: List[Int]) =>
    fromSeq(m.map(Pixel.diagonal)).isDiagonal
  }

  val transposeIsomorphism = forAll { (m: Maxel[Int]) =>
    m.transpose.transpose === m
  }

  val empty = prop(Maxel.empty[Int].isEmpty)

  val nonempty = forAll { (p: List[Pixel[Int]]) =>
    p.isEmpty == fromSeq(p).isEmpty
  }

  def realmLaws[A:Gen:Realm](s: String) = {
    import Realm._
    Properties.properties(s)(
      "commutativity" -> forAll(commutativeLaw[A] _),
      "absorption" -> forAll(absorptionLaw[A] _),
      "summation" -> forAll(summationLaw[A] _),
      "identity" -> forAll(identityLaw[A] _),
      "idempotence" -> forAll(idempotentLaw[A] _),
      "distributivity" -> forAll(distributiveLaw[A] _),
      "associativity" -> forAll(associativeLaw[A] _))
  }

  val realm = realmLaws[Maxel[Int]]("maxel realm")

  val multiplicativeSemigroup =
    forAll { (a: Maxel[Int], b: Maxel[Int], c: Maxel[Int]) =>
      a * (b * c) === (a * b) * c
    }

  val multiplicationDistributes =
    forAll { (a: Maxel[Int], b: Maxel[Int], c: Maxel[Int]) =>
      a * (b + c) === ((a * b) + (a * c))
    }

  val multiplicativeIdentity = forAll { (a: Maxel[Int], i: Int) =>
    (e(i,i) * a === a) === (a.cross.row subsetOf Set(i)) &&
    (a * e(i,i) === a) === (a.cross.col subsetOf Set(i))
  }

  val partialIdentities = forAll { (m: Maxel[Int], j: Set[Int]) =>
    val ej = partialIdentity(j)
    ((ej * m).cross.row subsetOf j) && ((m * ej).cross.col subsetOf j)
  }
}


