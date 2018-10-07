package maxel

import spire.algebra.Eq
import spire.syntax.all._

/** A pixel is a pair of values. */
case class Pixel[A](row: A, col: A) {

  /** `x.transpose.row == x.col && x.transpose.col == x.row` */
  def transpose: Pixel[A] = Pixel(col, row)

  /** A pixel `p` is diagonal exactly when `p.transpose == p` */
  def isDiagonal(implicit E: Eq[A]): Boolean = row === col

  /** Two pixels are column collinear when they are in the same column. */
  def columnCollinear(p: Pixel[A])(implicit E: Eq[A]): Boolean = col === p.col

  /** Two pixels are row collinear when they are in the same row. */
  def rowCollinear(p: Pixel[A])(implicit E: Eq[A]): Boolean = row === p.row

  /** Two pixels are collinear when they agree on the row or column. */
  def collinear(p: Pixel[A])(implicit E: Eq[A]): Boolean =
    rowCollinear(p) || columnCollinear(p)

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
    columnCollinear(p) && rowCollinear(p)

  /**
   * Multiply by a singleton on the right. If `k` equals this pixel's row,
   * returns this pixel's column.
   */
  def rightBy(k: A)(implicit E: Eq[A]): Option[A] =
    if (k === row) Some(col) else None

  /**
   * Multiply by a singleton on the left. If `k` equals this pixel's column,
   * returns this pixel's row.
   */
  def leftBy(k: A)(implicit E: Eq[A]): Option[A] =
    if (k === col) Some(row) else None
}

object Pixel {
  /** `diagonal(x).row == x == diagonal(x).col` */
  def diagonal[A](a: A): Pixel[A] = Pixel(a, a)

  implicit def pixelEq[A:Eq]: Eq[Pixel[A]] = new Eq[Pixel[A]] {
    def eqv(a: Pixel[A], b: Pixel[A]) = a === b
  }
}

