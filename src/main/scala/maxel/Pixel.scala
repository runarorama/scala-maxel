package maxel

import spire.algebra.Eq
import spire.syntax.all._

/** A pixel is a pair of values. */
case class Pixel[A](row: A, col: A) {

  /** `x.transpose.row == x.column && x.transpose.column == x.row` */
  def transpose: Pixel[A] = Pixel(col, row)

  /** A pixel `p` is diagonal exactly when `p.transpose == p` */
  def isDiagonal(implicit E: Eq[A]): Boolean = row === col

  /** Two pixels are column collinear when they are in the same column. */
  def columnCollinear(p: Pixel[A])(implicit E: Eq[A]): Boolean = col === p.col

  /** Two pixels are row collinear when they are in the same row. */
  def rowCollinear(p: Pixel[A])(implicit E: Eq[A]): Boolean = row === p.row

  /** Two pixels are collinear when they agree on the row or column. */
  def collinear(p: Pixel[A])(implicit E: Eq[A]): Boolean = row === p.row

  /**
   * Category multiplication on pixels. The composite pixel `p *? q` exists
   * exactly when `p.col == q.row`. The diagonals are the identities for this
   * operation.
   *
   * Transpose law: `p.transpose *? q.transpose == (p *? q).transpose`
   */
  def *?(p: Pixel[A])(implicit E: Eq[A]): Option[Pixel[A]] =
    if (col === p.row) Some(Pixel(row, p.col)) else None

  /** Two pixels are equal if they agree on both the row and column. */
  def ===(p: Pixel[A])(implicit E: Eq[A]): Boolean =
    row === p.row && col === p.col
}

object Pixel {
  /** `diagonal(x).row == x == diagonal(x).col` */
  def diagonal[A](a: A): Pixel[A] = Pixel(a, a)

  implicit def pixelEq[A:Eq]: Eq[Pixel[A]] = new Eq[Pixel[A]] {
    def eqv(a: Pixel[A], b: Pixel[A]) = a === b
  }
}

