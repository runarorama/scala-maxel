package maxel

import scalaprops._
import scalaprops.Gen._
import scalaprops.Property._
import scalaz.std.string._

object PixelTests extends Scalaprops {
  import maxel._
  import spire.algebra.Eq
  import spire.syntax.all._
  import spire.std.char._
  import spire.std.option._
  import Pixel._

  implicit val genChar: Gen[Char] = genCharAll

  implicit def genPixel[A:Gen:Eq]: Gen[Pixel[A]] = for {
    row <- Gen[A]
    col <- Gen[A]
  } yield Pixel(row, col)

  val transpose = forAll { p: Pixel[Char] =>
    p.transpose.row == p.col && p.transpose.col == p.row
  }

  val transposeDiagonal = forAll { p: Pixel[Char] =>
    p.transpose == p == p.isDiagonal
  }

  val category = Properties.properties("category")(
    "composition" -> forAll { (a: Char, b: Char, c: Char) =>
      Pixel(a,b) *? Pixel(b,c) === Option(Pixel(a,c)) &&
      Pixel(a,a) *? Pixel(b,b) === Option.empty[Pixel[Char]]
    },
    "associativity" ->
      forAll { (p: Pixel[Char], q: Pixel[Char], r: Pixel[Char]) =>
        (q *? r).flatMap(p *? _) === (p *? q).flatMap(_ *? r)
    },
    "identity" ->
      forAll { (p: Pixel[Char]) =>
        p *? diagonal(p.col) === Option(p) &&
        (diagonal(p.row) *? p === Option(p))
      }
  )

}
