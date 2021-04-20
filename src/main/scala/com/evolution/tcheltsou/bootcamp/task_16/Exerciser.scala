package com.evolution.tcheltsou.bootcamp.task_16

import cats.Parallel
import cats.effect.concurrent.Ref
import cats.effect.{Blocker, ContextShift, Sync}
import cats.implicits._

trait Exerciser[F[_]] {
  def sign: F[Ref[F, Option[Int]]]
}

object Exerciser {

  def of[F[_]: ContextShift: Sync: Parallel](reader: ConsoleReader[F]): Exerciser[F] = new Exerciser[F] {
    override def sign: F[Ref[F, Option[Int]]] =
      for {
        data <- reader.readFromFile
        seed <- reader.readSeed
        sign <- Signer.sign[F](data, seed)
        ref  <- Ref.of[F, Option[Int]](sign)
      } yield ref
  }

}
