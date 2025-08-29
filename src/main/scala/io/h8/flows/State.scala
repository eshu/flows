package io.h8.flows

import cats.data.NonEmptyChain

sealed trait State[-I, +O, +E] {
  private[flows] def <~[_I >: O, _O, _E >: E](that: State[_I, _O, _E]): State[I, _O, _E]

  private[flows] def ~>[_I, _O <: I, _E >: E](onDone: OnDone[_I, _O, _E]): State[_I, O, _E]
}

object State {
  final case class Success[-I, +O, +E](flow: Flow[I, O, E]) extends State[I, O, E] {
    private[flows] def <~[_I >: O, _O, _E >: E](that: State[_I, _O, _E]): State[I, _O, _E] = that match {
      case Success(nextFlow)   => Success(flow ~> nextFlow)
      case Complete            => Complete
      case failure: Failure[_] => failure
    }

    override private[flows] def ~>[_I, _O <: I, _E >: E](onDone: OnDone[_I, _O, _E]): State[_I, O, _E] =
      onDone.onSuccess() <~ this
  }

  final case object Complete extends State[Any, Nothing, Nothing] {
    private[flows] def <~[_I, _O, _E](state: State[_I, _O, _E]): State[Any, Nothing, _E] = state match {
      case failure: Failure[_] => failure
      case _                   => Complete
    }

    override private[flows] def ~>[_I, _O <: Any, _E >: Nothing](onDone: OnDone[_I, _O, _E]): State[_I, Nothing, _E] =
      onDone.onComplete() <~ this
  }

  final case class Failure[+E](failures: NonEmptyChain[Either[Exception, E]]) extends State[Any, Nothing, E] {
    private[flows] def <~[_I, _O, _E >: E](state: State[_I, _O, _E]): Failure[_E] = state match {
      case Failure(previous) => Failure(previous ++ failures)
      case _                 => this
    }

    override private[flows] def ~>[_I, _O <: Any, _E >: E](onDone: OnDone[_I, _O, _E]): State[_I, Nothing, _E] =
      onDone.onFailure() <~ this
  }

  def failure(e: Exception): Failure[Nothing] = Failure(NonEmptyChain(Left(e)))
}
