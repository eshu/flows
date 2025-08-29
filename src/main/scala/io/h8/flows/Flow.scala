package io.h8.flows

trait Flow[-I, +O, +E] extends (I => Yield[I, O, E]) {
  def apply(in: I): Yield[I, O, E]

  final def ~>[_I >: O, _O, _E >: E](that: Flow[_I, _O, _E]): Flow[I, _O, _E] = this(_) ~> that
}

object Flow {}
