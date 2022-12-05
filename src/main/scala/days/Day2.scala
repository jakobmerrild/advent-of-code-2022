package days

final case class Day2(input: String) extends Puzzle {
  trait Hand {
    def fight(other: Hand): Int
    def value: Int
    def choose(outcome: Outcome): Hand
  }

  trait Outcome {
    def value: Int
  }

  case object Loss extends Outcome {
    override def value: Int = 0
  }
  case object Draw extends Outcome {
    override def value: Int = 3
  }
  case object Win extends Outcome {
    override def value: Int = 6
  }

  case object Rock extends Hand {
    override def fight(other: Hand): Int = other match {
      case Rock     => Draw.value
      case Paper    => Loss.value
      case Scissors => Win.value
    }
    override val value: Int = 1
    override def choose(outcome: Outcome): Hand = outcome match {
      case Draw => Rock
      case Loss => Scissors
      case Win  => Paper
    }
  }
  case object Paper extends Hand {
    override def fight(other: Hand): Int = other match {
      case Paper    => Draw.value
      case Scissors => Loss.value
      case Rock     => Win.value
    }
    override def value: Int = 2
    override def choose(outcome: Outcome): Hand = outcome match {
      case Draw => Paper
      case Win  => Scissors
      case Loss => Rock
    }
  }
  case object Scissors extends Hand {
    override def fight(other: Hand): Int = other match {
      case Paper    => 6
      case Rock     => 0
      case Scissors => 3
    }
    override def value: Int = 3
    override def choose(outcome: Outcome): Hand = outcome match {
      case Draw => Scissors
      case Win  => Rock
      case Loss => Paper
    }
  }

  final case class Combat(theirs: Hand, mine: Hand)

  private def readHand(s: String): Option[Hand] =
    s match {
      case "A" | "X" => Some(Rock)
      case "B" | "Y" => Some(Paper)
      case "C" | "Z" => Some(Scissors)
      case _         => None
    }

  private def readOutcome(s: String): Option[Outcome] = s match {
    case "X" => Some(Loss)
    case "Y" => Some(Draw)
    case "Z" => Some(Win)
    case _   => None
  }

  private def readCombat(s: String): Option[Combat] = {
    s.split(" ").flatMap(readHand).toList match {
      case theirs :: mine :: Nil => Some(Combat(theirs, mine))
      case _                     => None
    }
  }

  final case class Combat2(theirs: Hand, outcome: Outcome)

  private def readCombat2(s: String): Option[Combat2] =
    s.split(" ").toList match
      case x :: y :: Nil =>
        for {
          theirs <- readHand(x)
          outcome <- readOutcome(y)
        } yield Combat2(theirs, outcome)
      case _ => None

  override def solve: String =
    input
      .split("\n")
      .flatMap(readCombat)
      .map { case Combat(theirs, mine) => mine.fight(theirs) + mine.value }
      .sum
      .toString
  override def solve2: String =
    input
      .split("\n")
      .flatMap(readCombat2)
      .map { case Combat2(theirs, outcome) =>
        theirs.choose(outcome).value + outcome.value
      }
      .sum
      .toString
}
