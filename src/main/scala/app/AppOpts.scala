package app

import com.monovore.decline.Opts

object AppOpts {
  final case class Solve(day: Int)
  lazy val solve: Opts[Solve] =
    Opts.subcommand("solve", "solves the challenge for the given day")(Opts.argument[Int]("day").map(Solve.apply))
}
