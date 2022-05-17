package ice_cream

import shapeless.{HList, ::, HNil}

object Encoders {

  // Turn a value of type A into a row of cells in a CSV file:
  trait CsvEncoder[A] {
    import CsvEncoder._

    def encode(value: A): List[String]

    def writeCsv(values: List[A])(implicit enc: CsvEncoder[A]): String =
      values.map(value => enc.encode(value).mkString(",")).mkString("\n")



    //Ex.
//    val reprEncoder: CsvEncoder[String :: Int :: Boolean :: HNil] =
//      implicitly
//    reprEncoder.encode("abc" :: 123 :: true :: HNil)
    // res9: List[String] = List(abc, 123, yes)
  }


  object CsvEncoder {
    // "Summoner" method
    def apply[A](implicit enc: CsvEncoder[A]): CsvEncoder[A] =
      enc

    // "Constructor" method
    def instance[A](func: A => List[String]): CsvEncoder[A] =
      new CsvEncoder[A] {
        def encode(value: A): List[String] =
          func(value)
      }

    implicit val hnilEncoder: CsvEncoder[HNil] =
      instance(hnil => Nil)

    implicit def hlistEncoder[H, T <: HList](
                                              implicit
                                              hEncoder: CsvEncoder[H],
                                              tEncoder: CsvEncoder[T]
                                            ): CsvEncoder[H :: T] =
      instance {
        case h :: t =>
          hEncoder.encode(h) ++ tEncoder.encode(t)
      }


    implicit val stringEncoder: CsvEncoder[String] =
      instance(str => List(str))
    implicit val intEncoder: CsvEncoder[Int] =
      instance(num => List(num.toString))
    implicit val booleanEncoder: CsvEncoder[Boolean] =
      instance(bool => List(if(bool) "yes" else "no"))
  }

}
