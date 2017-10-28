package ice_cream

import Encoders.CsvEncoder
import Encoders.CsvEncoder._
import shapeless.Generic

object Examples {

  case class IceCream(name: String, numCherries: Int, inCone: Boolean)

  object IceCream {
    val iceCreamSimpleEncoder: CsvEncoder[IceCream] =
      new CsvEncoder[IceCream] {
        def encode(i: IceCream): List[String] =
          List(
            i.name,
            i.numCherries.toString,
            if (i.inCone) "yes" else "no"
          )
      }

    implicit val iceCreamEncoderFromHList: CsvEncoder[IceCream] = {
      val gen = Generic[IceCream]     // a converter between IceCream and its HList representation in both directions
      // If we get an error for `gen`, it means it is a type that isn’t a case class or a sealed abstract type.

      // Since we have `implicit` `CsvEncoder[String]`, `CsvEncoder[Int]`, `CsvEncoder[Boolean]`, and `CsvEncoder[HList]`,
      // we can derive `CsvEncoder[String :: Int :: Boolean :: HNil]`, where `String :: Int :: Boolean :: HNil` is
      // the type HList type of the IceCream, which type alias is `gen.Repr`.
      val enc = CsvEncoder[gen.Repr]    // CsvEncoder is a type class

      // `enc.encode()` takes an instance of String :: Int :: Boolean :: HNil, and `gen.to()` gives exactly that instance
      instance(iceCream => enc.encode(gen.to(iceCream)))
    }

    /**
      * Given a type A and an HList type R, an implicit Generic to map A to R, and a CsvEncoder for R,
      * create a CsvEncoder for A.
      */
    implicit def genericEncoder[A, R](
                                       implicit
                                       gen: Generic.Aux[A, R],    // type Aux[A, R] = Generic[A] { type Repr = R }
                                       enc: CsvEncoder[R]
                                     ): CsvEncoder[A] =
      instance(a => enc.encode(gen.to(a)))
  }



  def main(args: Array[String]): Unit = {
    val iceCreamEncoder = CsvEncoder[IceCream]
    println(
      iceCreamEncoder.writeCsv(List(IceCream("Stracatela", 2, true), IceCream("Chocolate", 2, true)))
    )
  }
}