package io.h8.flows

trait Flow[-I, +O, +E] extends (I => Yield[I, O, E]) {
  def apply(in: I): Yield[I, O, E]

  final def ~>[_I >: O, _O, _E >: E](that: Flow[_I, _O, _E]): Flow.Safe[I, _O, _E] = this.safe(_) ~> that

  private[flows] def safe(in: I): Yield[I, O, E] =
    try this(in)
    catch { case e: Exception => Yield.None(OnDone.OnFailure(e)) }
}

object Flow {
  trait Safe[-I, +O, +E] extends Flow[I, O, E] {
    final override private[flows] def safe(in: I): Yield[I, O, E] = this(in)
  }
}
