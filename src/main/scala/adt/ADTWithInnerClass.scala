package adt

import shapeless.{:+:, CNil, Coproduct, Generic, Inl, Inr}

object ADTWithInnerClass {
  sealed abstract class T
  sealed class M extends T
  sealed class N extends T
  final case class A(s: String) extends M
  final case class B(b: Boolean) extends N

  case class Payload[+T](t: T, n: Int)

  trait Q[S] {
    def process(p: Payload[S]): Unit
  }

  implicit val q_a = new Q[A] {
    override def process(s: Payload[A]): Unit = print(s"A: $s")
  }

  implicit val q_b = new Q[B] {
    override def process(s: Payload[B]): Unit = print(s"B: $s")
  }

  def createQ[S](f: Payload[S] => Unit): Q[S] = new Q[S] {
    override def process(s: Payload[S]): Unit = f(s)
  }

  case class Wrapper(w: String)



  // shapeless -----------------------------
  implicit val cnilEncoder: Q[CNil] =
    createQ(t => throw new Exception("Inconceivable!"))

  implicit def coproductReprQ[H, C <: Coproduct](implicit
                                                 hQ: Q[H],
                                                 tQ: Q[C]): Q[H :+: C] = createQ {
    payload =>
      payload.t match {
        case Inl(h) => hQ.process(payload.copy(t = h))
        case Inr(t) => tQ.process(payload.copy(t = t))
      }
  }

  implicit def coproductQ[S, C](implicit
                                gen: Generic.Aux[S, C],
                                repr: Q[C]
                               ): Q[S] =
    createQ(payload => repr.process(payload.copy(t = gen.to(payload.t))))
  // ------------------------------------------


  def q(p: Payload[T])(implicit proc: Q[T]) = proc.process(p)

  val t: Payload[T] = Payload(B(false), 0)
  println(q(t))
}
