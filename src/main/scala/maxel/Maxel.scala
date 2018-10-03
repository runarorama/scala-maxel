package maxel

import mset._
import spire.math.Natural
import spire.algebra.Eq
import spire.algebra.Order
import spire.syntax.all._
import spire.compat._
import Realm.NaturalRealm

/** A maxel is a multiset of pixels. */
case class Maxel[A](rep: MSet[Natural, Pixel[A]]) {

  /** The size of a maxel is its number of pixels. */
  def size = rep.size

  /** The frame of a maxel is the set of its pixels. */
  def frame: Set[Pixel[A]] = rep.toSet

  /**
   * The extent of a maxel is the largest row and column of any of its pixels,
   * respectively. This is a partial functor from maxel space into pixel space.
   * The extent of the empty maxel is undefined.
   */
  def extent(implicit O: Order[A]): Option[Pixel[A]] = {
    val ps = frame
    if (isEmpty) None else Some(Pixel(ps.maxBy(_.row).row, ps.maxBy(_.col).col))
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
}

