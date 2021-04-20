package com.evolution.tcheltsou.bootcamp.task_16

import cats.Parallel
import cats.effect.{Blocker, ContextShift, ExitCode, IO, IOApp, Resource, Sync}

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {

    val resultResource = for {
      blocker   <- Blocker[IO]
      console   <- Resource.liftF(Console[IO])
      reader    <- Resource.liftF(ConsoleReader.of[IO](console))
      exerciser <- Resource.liftF(IO.delay({println(Thread.currentThread().getName);Exerciser.of(reader)}))
      _         <- Resource.liftF(IO.contextShift(blocker.blockingContext).shift)
      ref       <- Resource.liftF({println(Thread.currentThread().getName);exerciser.sign})
      sign      <- Resource.liftF({println(Thread.currentThread().getName);ref.get})
    } yield sign

    resultResource.use(sign => IO(println(s"Got it: ${Thread.currentThread().getName}"))).as[ExitCode](ExitCode.Success)
  }
}
