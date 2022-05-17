import shapeless.{HList, ::, HNil}
import shapeless.ops.hlist.Last

object DependentlyTypedFunc {

  trait Second[L <: HList] {
    type Out
    def apply(value: L): Out
  }

  object Second {
    type Aux[L <: HList, O] = Second[L] { type Out = O }
    def apply[L <: HList](implicit inst: Second[L]): Aux[L, inst.Out] =
      inst

    implicit def hlistSecond[A, B, Rest <: HList]: Aux[A :: B :: Rest, B] =
      new Second[A :: B :: Rest] {
        type Out = B
        def apply(value: A :: B :: Rest): B =
          value.tail.head
      }
  }

  val second1 = Second[String :: Boolean :: Int :: HNil]
    // second1: Second[shapeless.::[String,shapeless.::[Boolean,shapeless
    //  .::[Int,shapeless.HNil]]]]{type Out = Boolean} = $anon$1@480e7a96

  val sec = second1("foo" :: true :: 123 :: HNil)
  // res10: second1.Out = true

}
