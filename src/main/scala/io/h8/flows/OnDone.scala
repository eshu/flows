package io.h8.flows

trait OnDone[-I, +O, +E] {
  self =>

  def onSuccess(): State[I, O, E]
  def onComplete(): State[I, O, E]
  def onFailure(): State[I, O, E]

  private[flows] def safe: OnDone.Safe[I, O, E] = new OnDone.Safe[I, O, E] {
    override def onSuccess(): State[I, O, E] =
      try self.onSuccess()
      catch { case e: Exception => State.failure(e) }

    override def onComplete(): State[I, O, E] =
      try self.onComplete()
      catch { case e: Exception => State.failure(e) }

    override def onFailure(): State[I, O, E] =
      try self.onFailure()
      catch { case e: Exception => State.failure(e) }
  }
}

object OnDone {
  trait Safe[-I, +O, +E] extends OnDone[I, O, E] {
    self =>

    final private[flows] def <~[_I >: O, _O, _E >: E](that: OnDone[_I, _O, _E]): OnDone.Safe[I, _O, _E] =
      new OnDone.Safe[I, _O, _E] {
        def onSuccess(): State[I, _O, _E]  = that.onSuccess() ~> self
        def onComplete(): State[I, _O, _E] = that.onComplete() ~> self
        def onFailure(): State[I, _O, _E]  = that.onFailure() ~> self
      }

    final override private[flows] def safe: Safe[I, O, E] = this
  }

  final case class DoNothing[-I, +O, +E](flow: Flow[I, O, E]) extends OnDone[I, O, E] {
    def onSuccess(): State[I, O, E]  = State.Success(flow)
    def onComplete(): State[I, O, E] = State.Complete
    def onFailure(): State[I, O, E]  = State.Complete
  }

  final case class OnFailure[+E](e: Exception) extends OnDone[Any, Nothing, E] {
    def onSuccess(): State.Failure[E]  = State.failure(e)
    def onComplete(): State.Failure[E] = State.failure(e)
    def onFailure(): State.Failure[E]  = State.failure(e)
  }
}
