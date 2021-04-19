package com.evolution.tcheltsou.bootcamp.task_16

import cats.Parallel
import cats.effect.{Blocker, ExitCode, IO, IOApp, Resource, Sync}

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {

    val resultResource = for {
      blocker   <- Blocker[IO]
      console   <- Resource.liftF(Console[IO])
      reader    <- Resource.liftF(ConsoleReader.of[IO](console))
      cs         = IO.contextShift(blocker.blockingContext)
      exerciser <- Resource.liftF(IO.pure(Exerciser.of(reader)(cs, Sync[IO], Parallel[IO])))
      ref       <- Resource.liftF(exerciser.sign(blocker))
      sign      <- Resource.liftF(ref.get)
    } yield sign

    resultResource.use(sign => IO(println(s"Got it: $sign"))).as[ExitCode](ExitCode.Success)
  }
}
