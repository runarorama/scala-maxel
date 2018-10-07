package maxel

import scalaprops._
import scalaprops.Gen._
import scalaprops.Property._
import scalaz.std.string._

object MaxelTests extends Scalaprops {
  import maxel._
  import spire.algebra.{Eq, MultiplicativeMonoid, AdditiveMonoid}
  import spire.syntax.all._
  import spire.std.char._
  import spire.std.int._
  import spire.std.boolean._
  import spire.std.option._
  import mset.Realm
  import Maxel._
  import PixelTests._
  import mset._

  implicit def genMaxel[A:Gen:Eq:MultiplicativeMonoid:AdditiveMonoid,
                        I:Gen:Eq]: Gen[Maxel[I,A]] =
    Gen[List[Pixel[I]]].map(fromSeq[I,A])

  val extentFunctorality = forAll { m: Maxel[Int,Int] =>
    m.transpose.extent === m.extent.map(_.transpose)
  }

  val intentFunctorality = forAll { m: Maxel[Int,Int] =>
    m.transpose.intent === m.intent.map(_.transpose)
  }

  val constructiveDiagonality = forAll { (m: List[Int]) =>
    fromSeq[Int,Int](m.map(Pixel.diagonal)).isDiagonal
  }

  val transposeIsomorphism = forAll { (m: Maxel[Int,Int]) =>
    m.transpose.transpose === m
  }

  val empty = prop(Maxel.empty[Int,Int].isEmpty)

  val nonempty = forAll { (p: List[Pixel[Int]]) =>
    p.isEmpty == fromSeq[Int,Int](p).isEmpty
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

  val realm = realmLaws[Maxel[Int,Int]]("maxel realm")

  val multiplicativeSemigroup =
    forAll { (a: Maxel[Int,Int], b: Maxel[Int,Int], c: Maxel[Int,Int]) =>
      a * (b * c) === (a * b) * c
    }

  val multiplicationDistributes =
    forAll { (a: Maxel[Int,Int], b: Maxel[Int,Int], c: Maxel[Int,Int]) =>
      a * (b + c) === ((a * b) + (a * c))
    }

  val multiplicativeIdentity = forAll { (a: Maxel[Int,Int], i: Int) =>
    (e[Int,Int](i,i) * a === a) === (a.cross.row subsetOf Set(i)) &&
    (a * e[Int,Int](i,i) === a) === (a.cross.col subsetOf Set(i))
  }

  val partialIdentities = forAll { (m: Maxel[Int,Int], j: Set[Int]) =>
    val ej = partialIdentity[Int,Int](j)
    ((ej * m).cross.row subsetOf j) && ((m * ej).cross.col subsetOf j)
  }

  val colVexel = forAll { (m: Maxel[Int,Int], j: Int) =>
    m.col(j) === m.rep.flatMap(x => MSet.fromSeq(x.leftBy(j).toSeq))
  }

  val rowVexel = forAll { (m: Maxel[Int,Int], j: Int) =>
    m.row(j) === m.rep.flatMap(x => MSet.fromSeq(x.rightBy(j).toSeq))
  }
}


