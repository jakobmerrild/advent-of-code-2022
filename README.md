# advent-of-code-2022
This repository contains my solutions for [advent-of-code 2022](https://adventofcode.com/2022).

The solutions are written in Scala 3

## How to run
There's a VS Code workspace and for each available solution it's possible to simply modify the
run configuration to solve for the desired day.

Alternatively you can run the the app, e.g. via `sbt`
```
sbt run solve <day>
```

Optionally you can provide the path to a different input
```
sbt run solve <day> --path <pathToInput>
```