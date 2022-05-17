package adt

import shapeless.{:+:, CNil, Coproduct, Generic, Inl, Inr}

object ADTExample {
  sealed abstract class T
  final case class A(s: String) extends T
  final case class B(b: Boolean) extends T

  trait Q[S] {
    def process(s: S): Unit
  }

  implicit val q_a = new Q[A] {
    override def process(s: A): Unit = print(s"A: $s")
  }

  implicit val q_b = new Q[B] {
    override def process(s: B): Unit = print(s"B: $s")
  }


  def createQ[S](f: S => Unit): Q[S] = new Q[S] {
    override def process(s: S): Unit = f(s)
  }

  // shapeless -----------------------------
  implicit val cnilEncoder: Q[CNil] =
    createQ(t => throw new Exception("Inconceivable!"))

  implicit def coproductReprQ[H, C <: Coproduct](implicit
                                                 hQ: Q[H],
                                                 tQ: Q[C]): Q[H :+: C] = createQ {
    case Inl(h) => hQ.process(h)
    case Inr(t) => tQ.process(t)
  }

  implicit def coproductQ[S, C](implicit
                                gen: Generic.Aux[S, C],
                                repr: Q[C]
                               ): Q[S] =
    createQ(a => repr.process(gen.to(a)))
  // ------------------------------------------


  def q(t: T)(implicit proc: Q[T]) = proc.process(t)

  val t: T = B(true)
  println(q(t))
}
