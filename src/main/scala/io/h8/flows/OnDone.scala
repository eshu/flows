package io.h8.flows

trait OnDone[-I, +O, +E] {
  self =>

  def onSuccess(): State[I, O, E]
  def onComplete(): State[I, O, E]
  def onFailure(): State[I, O, E]

  final private[flows] def <~[_I >: O, _O, _E >: E](that: OnDone[_I, _O, _E]): OnDone[I, _O, _E] =
    new OnDone[I, _O, _E] {
      override def onSuccess(): State[I, _O, _E]  = that.onSuccess() ~> self
      override def onComplete(): State[I, _O, _E] = that.onComplete() ~> self
      override def onFailure(): State[I, _O, _E]  = that.onFailure() ~> self
    }
}

object OnDone {
  final case class DoNothing[-I, +O, +E](flow: Flow[I, O, E]) extends OnDone[I, O, E] {
    override def onSuccess(): State[I, O, E]  = State.Success(flow)
    override def onComplete(): State[I, O, E] = State.Complete
    override def onFailure(): State[I, O, E]  = State.Complete
  }
}
