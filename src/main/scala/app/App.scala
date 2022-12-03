package app

import com.monovore.decline.effect.CommandIOApp
import cats.effect.ExitCode
import cats.effect.IO
import com.monovore.decline.Opts
import cats.data.Reader.apply

object App extends CommandIOApp("advent-of-code", "Run advent of code solutions", true, "0.0.0"){

  override def main: Opts[IO[ExitCode]] = 
    AppOpts.solve.map(solve)
  

  def solve(solve: AppOpts.Solve): IO[ExitCode] =
    input.Reader.getInputForDay(solve.day).flatMap { input =>
        IO(print("foo")) *> IO(print(input.mkString("\n"))).as(ExitCode.Success)
    }
}
