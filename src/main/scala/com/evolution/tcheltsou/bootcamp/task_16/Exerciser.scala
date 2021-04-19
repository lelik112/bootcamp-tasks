package com.evolution.tcheltsou.bootcamp.task_16

import cats.Parallel
import cats.effect.concurrent.Ref
import cats.effect.{Blocker, ContextShift, Sync}
import cats.implicits._

trait Exerciser[F[_]] {
  def sign(blocker: Blocker): F[Ref[F, Option[Int]]]
}

object Exerciser {

  def of[F[_]: ContextShift: Sync: Parallel](reader: ConsoleReader[F]): Exerciser[F] =
    (blocker: Blocker) => for {
      data <- reader.readFromFile(blocker)
      seed <- reader.readSeed(blocker)
      sign <- ContextShift[F].evalOn(blocker.blockingContext)(Signer.sign[F](data, seed))
      ref <- Ref.of[F, Option[Int]](sign)
    } yield ref
}
