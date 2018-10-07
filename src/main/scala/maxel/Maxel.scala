package maxel

import  mset._
import  spire.algebra.{AdditiveMonoid, Eq, MultiplicativeMonoid,
                       MultiplicativeSemigroup}
import  spire.algebra.lattice.{JoinSemilattice, MeetSemilattice}
import  spire.algebra.Order
import  spire.math._
import  spire.compat._
import  spire.syntax.all._

/**
 * Maxels are a generalization of matrices. A matrix can be viewed as an image
 * of a maxel. Matrices and maxels are in an adjunction, where a map from
 * maxels to matrices is a right adjoint.
 *
 * A maxel is represented by a multiset of pixels. The "location" of the pixel
 * is in `I`, while the value of the pixels is in `A`.
 */
case class Maxel[I,A](rep: MSet[A, Pixel[I]]) {
  import MSet._

  /** The size of a maxel is its number of pixels. */
  def size(implicit M: AdditiveMonoid[A]): A = rep.size

  /**
   * The [[frame]] of a maxel is the set of its pixels, also known as its
   * [[support]].
   */
  def frame: Set[Pixel[I]] = rep.toSet

  /**
   * The [[support]] of a maxel is the set of its pixels, also known as its
   * [[frame]].
   */
  def support: Set[Pixel[I]] = frame

  /**
   * The extent of a maxel is the largest row and column of any of its pixels,
   * respectively. The extent of the empty maxel is undefined.
   */
  def extent(implicit O: Order[I],
                      M: AdditiveMonoid[A],
                      A: Eq[A]): Option[Pixel[I]] = {
    val ps = frame
    if (isEmpty) None else Some(Pixel(ps.maxBy(_.row).row, ps.maxBy(_.col).col))
  }

  /**
   * The intent of a maxel is the smallest row and column of any of its pixels,
   * respectively. The intent of the empty maxel is undefined.
   */
  def intent(implicit O: Order[I],
                      M: AdditiveMonoid[A],
                      E: Eq[A]): Option[Pixel[I]] = {
    val ps = frame
    if (isEmpty) None else Some(Pixel(ps.minBy(_.row).row, ps.minBy(_.col).col))
  }

  /** A maxel is diagonal when all its pixels are diagonal. */
  def isDiagonal(implicit E: Eq[I]): Boolean = frame.forall(_.isDiagonal)

  /**
   * The transpose of a maxel is the maxel of transposes of its pixels.
   */
  def transpose(implicit M: MultiplicativeMonoid[A],
                         N: AdditiveMonoid[A],
                         E: Eq[A]): Maxel[I,A] =
    Maxel(rep.map(_.transpose))

  /**
   * A maxel is symmetric if it is isomorphic to its transpose.
   */
  def symmetric(implicit E: Eq[I], A: Eq[A],
                         M: MultiplicativeMonoid[A],
                         N: AdditiveMonoid[A]): Boolean = transpose === this

  /**
   * Two maxels are equal when they consist of the same pixels in the same
   * measure.
   */
  def ===(m: Maxel[I,A])(implicit E: Eq[I],
                                  A: Eq[A],
                                  M: AdditiveMonoid[A]): Boolean =
    Eq[MSet[A,Pixel[I]]].eqv(rep, m.rep)

  /**
   * A maxel is empty if it contains no pixels. An empty maxel's size is zero.
   */
  def isEmpty(implicit E: Eq[I], A: Eq[A], M: AdditiveMonoid[A]): Boolean =
    rep.isEmpty

  /**
   * A pixel appears in the union of two maxels a number of times equal to its
   * appearances in one maxel or the other, whichever has more.
   */
  def union(m: Maxel[I,A])(implicit L: JoinSemilattice[A],
                                    M: AdditiveMonoid[A],
                                    E: Eq[A]): Maxel[I,A] =
    Maxel(rep union m.rep)

  /** An alias for `union`. */
  def ∪(m: Maxel[I,A])(implicit L: JoinSemilattice[A],
                                    M: AdditiveMonoid[A],
                                    E: Eq[A]): Maxel[I,A] = union(m)

  /**
   * A pixel appears in the intersection of two maxels a number of times equal
   * to its appearances in one maxel or the other, whichever has fewer.
   */
  def intersect(m: Maxel[I,A])(implicit L: MeetSemilattice[A],
                                        M: AdditiveMonoid[A],
                                        E: Eq[A]): Maxel[I,A] =
    Maxel(rep intersect m.rep)

  /** An alias for `intersect`. */
  def ∩(m: Maxel[I,A])(implicit L: MeetSemilattice[A],
                                M: AdditiveMonoid[A],
                                E: Eq[A]): Maxel[I,A] = intersect(m)

  /**
   * A pixel appears in the sum of two maxels a number of times equal to the
   * sum of its appearances in either maxel.
   */
  def add(m: Maxel[I,A])(implicit A: AdditiveMonoid[A],
                                  E: Eq[A]): Maxel[I,A] =
    Maxel(rep sum m.rep)

  /** An alias for `add`. */
  def +(m: Maxel[I,A])(implicit A: AdditiveMonoid[A],
                                E: Eq[A]): Maxel[I,A] = add(m)

  /**
   * Subtract one maxel from another. Subtracts the appearances of a pixel in
   * the second maxel from its appearances in the first.
   */
  def subtract(m: Maxel[I,A])(implicit R: MRealm[A]): Maxel[I,A] =
    Maxel(rep difference m.rep)

  /** Scales a maxel by duplicating all its pixels. */
  def scale(n: A)(implicit M: MultiplicativeSemigroup[A],
                           N: AdditiveMonoid[A],
                           E: Eq[A]): Maxel[I,A] =
    Maxel(rep scale n)

  /** The product of two maxels is the product of all their pixels. */
  def multiply(m: Maxel[I,A])(implicit E: Eq[I],
                                       F: Eq[A],
                                       M: MultiplicativeMonoid[A],
                                       A: AdditiveMonoid[A]): Maxel[I,A] =
    Maxel(for {
      p <- rep
      q <- m.rep
      r <- MSet.fromSeq((p *? q).toSeq)
    } yield r)

  /** The product of two maxels is the product of all their pixels. */
  def *(m: Maxel[I,A])(implicit E: Eq[I],
                                       F: Eq[A],
                                       M: MultiplicativeMonoid[A],
                                       A: AdditiveMonoid[A]): Maxel[I,A] =
    multiply(m)

  /** Get the value at a particular pixel. */
  def screen(p: Pixel[I])(implicit M: AdditiveMonoid[A]): A = rep(p)

  /** Get a 2D display of the maxel, starting at pixel 1,1 */
  def display(implicit I: Integral[I],
                       O: Order[I],
                       A: AdditiveMonoid[A],
                       E: Eq[A]): List[List[A]] =
    extent match {
      case Some(Pixel(rows, cols)) =>
        List.range(I.one, I.plus(rows, I.one)).map { row =>
          List.range(I.one, I.plus(cols, I.one)).map { col =>
            screen(Pixel(row, col))
          }
        }
      case None => List()
    }

  /** Get a string representation of the 2D display of the maxel. */
  def render(implicit I: Integral[I],
                      O: Order[I],
                      M: AdditiveMonoid[A],
                      E: Eq[A]): String =
    display.map(_.mkString("\t")).mkString("\n")

  /**
   * Get all the pixels in the cross of another pixel. The cross of a pixel
   * is all pixels in its row and column.
   */
  def cross(a: Pixel[I])(implicit E: Eq[I], M: MRealm[A]): Maxel[I,A] =
    Maxel(rep.filter(p => p.col === a.col || p.row === a.row))

  /**
   * The cross of a maxel is sets of rows and columns, that is, the set of
   * crosses of its pixels.
   */
  def cross: Pixel[Set[I]] = rep.foldLeft(Pixel(Set[I](), Set[I]())) {
    case (Pixel(rows, cols), (Pixel(row, col), _)) =>
      Pixel(rows + row, cols + col)
  }
}

object Maxel {
  def fromSeq[I,A](s: Seq[Pixel[I]])(
                   implicit M: MultiplicativeMonoid[A],
                            A: AdditiveMonoid[A],
                            E: Eq[A]): Maxel[I,A] =
    Maxel(MSet.fromSeq(s))

  def singleton[I,A](p: Pixel[I])(implicit M: MultiplicativeMonoid[A],
                                           A: AdditiveMonoid[A],
                                           E: Eq[A]): Maxel[I,A] =
    fromSeq(Seq(p))

  def e[I,A](row: I, col: I)(implicit M: MultiplicativeMonoid[A],
                                      A: AdditiveMonoid[A],
                                      E: Eq[A]): Maxel[I,A] =
    singleton(Pixel(row, col))

  /** The empty maxel with no pixels. Its size is zero. */
  def empty[I,A]: Maxel[I,A] = Maxel(MSet.empty)

  implicit def maxelRealm[I:Eq,A:Realm]: Realm[Maxel[I,A]] = new Realm[Maxel[I,A]] {
    def join(a: Maxel[I,A], b: Maxel[I,A]) = a ∪ b
    def meet(a: Maxel[I,A], b: Maxel[I,A]) = a ∩ b
    val zero = empty[I,A]
    def plus(a: Maxel[I,A], b: Maxel[I,A]) = a + b
    override def eqv(a: Maxel[I,A], b: Maxel[I,A]) = a === b
  }

  def partialIdentity[I,A](s: Set[I])(implicit M: MultiplicativeMonoid[A],
                                               A: AdditiveMonoid[A],
                                               E: Eq[A]): Maxel[I,A] =
    fromSeq(s.toSeq.map(Pixel.diagonal))
}

