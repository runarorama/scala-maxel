package maxel

import mset._
import spire.algebra.Eq
import spire.algebra.Order
import spire.syntax.all._
import spire.math._
import spire.compat._
import Realm.NaturalRealm

/**
 * Maxels are a generalization of matrices. A matrix can be viewed as an image
 * (a kind of colimit) of a maxel. Matrices and maxels are in an adjunction,
 * where a map from maxels to matrices is a right adjoint.
 *
 * A maxel is represented by a multiset of pixels.
 */
case class Maxel[A](rep: MSet[Natural, Pixel[A]]) {

  /** The size of a maxel is its number of pixels. */
  def size = rep.size

  /**
   * The [[frame]] of a maxel is the set of its pixels, also known as its
   * [[support]].
   */
  def frame: Set[Pixel[A]] = rep.toSet

  /**
   * The [[support]] of a maxel is the set of its pixels, also known as its
   * [[frame]].
   */
  def support: Set[Pixel[A]] = frame

  /**
   * The extent of a maxel is the largest row and column of any of its pixels,
   * respectively. The extent of the empty maxel is undefined.
   */
  def extent(implicit O: Order[A]): Option[Pixel[A]] = {
    val ps = frame
    if (isEmpty) None else Some(Pixel(ps.maxBy(_.row).row, ps.maxBy(_.col).col))
  }

  /**
   * The intent of a maxel is the smallest row and column of any of its pixels,
   * respectively. The intent of the empty maxel is undefined.
   */
  def intent(implicit O: Order[A]): Option[Pixel[A]] = {
    val ps = frame
    if (isEmpty) None else Some(Pixel(ps.minBy(_.row).row, ps.minBy(_.col).col))
  }

  /** A maxel is diagonal when all its pixels are diagonal. */
  def isDiagonal(implicit E: Eq[A]): Boolean = frame.forall(_.isDiagonal)

  /**
   * The transpose of a maxel is the maxel of transposes of its pixels.
   */
  def transpose: Maxel[A] = Maxel(rep.map(_.transpose))

  /**
   * A maxel is symmetric if it is isomorphic to its transpose.
   */
  def symmetric(implicit E: Eq[A]): Boolean = transpose === this

  /**
   * Two maxels are equal when they consist of the same pixels in the same
   * measure.
   */
  def ===(m: Maxel[A])(implicit E: Eq[A]): Boolean = rep === m.rep

  /**
   * A maxel is empty if it contains no pixels. An empty maxel's size is zero.
   */
  def isEmpty(implicit E: Eq[A]): Boolean = rep.isEmpty

  /**
   * A pixel appears in the union of two maxels a number of times equal to its
   * appearances in one maxel or the other, whichever has more.
   */
  def union(m: Maxel[A]): Maxel[A] = Maxel(rep union m.rep)

  /** An alias for `union`. */
  def ∪(m: Maxel[A]): Maxel[A] = union(m)

  /**
   * A pixel appears in the intersection of two maxels a number of times equal
   * to its appearances in one maxel or the other, whichever has fewer.
   */
  def intersect(m: Maxel[A]): Maxel[A] = Maxel(rep intersect m.rep)

  /** An alias for `intersect`. */
  def ∩(m: Maxel[A]): Maxel[A] = intersect(m)

  /**
   * A pixel appears in the sum of two maxels a number of times equal to the
   * sum of its appearances in either maxel.
   */
  def add(m: Maxel[A]): Maxel[A] = Maxel(rep sum m.rep)

  /** An alias for `add`. */
  def +(m: Maxel[A]): Maxel[A] = add(m)

  /**
   * Subtract one maxel from another. Subtracts the appearances of a pixel in
   * the second maxel from its appearances in the first.
   */
  def subtract(m: Maxel[A]): Maxel[A] = Maxel(rep difference m.rep)

  /** Scales a maxel by duplicating all its pixels. */
  def scale(n: Natural): Maxel[A] = Maxel(rep scale n)

  /** The product of two maxels is the product of all their pixels. */
  def multiply(m: Maxel[A])(implicit E: Eq[A]): Maxel[A] = Maxel(for {
    p <- rep
    q <- m.rep
    r <- MSet.multisetFromSeq((p *? q).toSeq)
  } yield r)

  /** The product of two maxels is the product of all their pixels. */
  def *(m: Maxel[A])(implicit E: Eq[A]): Maxel[A] = multiply(m)

  /** Get the value at a particular pixel. */
  def screen(p: Pixel[A]): Natural = rep(p)

  /** Get a 2D display of the maxel, starting at pixel 1,1 */
  def display(implicit I: Integral[A], O: Order[A]): List[List[Natural]] =
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
  def render(implicit I: Integral[A], O: Order[A]): String =
    display.map(_.mkString("\t")).mkString("\n")

  /**
   * Get all the pixels in the cross of another pixel. The cross of a pixel
   * is all pixels in its row and column.
   */
  def cross(a: Pixel[A])(implicit E: Eq[A]): Maxel[A] =
    Maxel(rep.filter(p => p.col === a.col || p.row === a.row))

  /**
   * The cross of a maxel is sets of rows and columns, that is, the set of
   * crosses of its pixels.
   */
  def cross: Pixel[Set[A]] = rep.foldLeft(Pixel(Set[A](), Set[A]())) {
    case (Pixel(rows, cols), (Pixel(row, col), _)) =>
      Pixel(rows + row, cols + col)
  }
}

object Maxel {
  def fromSeq[A](s: Seq[Pixel[A]]): Maxel[A] =
    Maxel(MSet.multisetFromSeq(s))

  def singleton[A](p: Pixel[A]): Maxel[A] = fromSeq(Seq(p))

  def e[A](row: A, col: A): Maxel[A] = singleton(Pixel(row, col))

  /** The empty maxel with no pixels. Its size is zero. */
  def empty[A]: Maxel[A] = Maxel(MSet.empty)

  implicit def maxelRealm[A:Eq]: Realm[Maxel[A]] = new Realm[Maxel[A]] {
    def join(a: Maxel[A], b: Maxel[A]) = a ∪ b
    def meet(a: Maxel[A], b: Maxel[A]) = a ∩ b
    val zero = empty[A]
    def plus(a: Maxel[A], b: Maxel[A]) = a + b
    override def eqv(a: Maxel[A], b: Maxel[A]) = a === b
  }

  def partialIdentity[A](s: Set[A]): Maxel[A] =
    fromSeq(s.toSeq.map(Pixel.diagonal))
}

