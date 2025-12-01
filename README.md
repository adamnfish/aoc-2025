Advent of Code 2025
===================

## Usage

### Run a solution
```bash
sbt "run <day> <part> <input-file>"
```

For example:
```bash
sbt "run 1 1 example"  # Run day 1, part 1 with example input
sbt "run 1 2 input"    # Run day 1, part 2 with real input
```

### Fetch puzzle input
First, get your session cookie from the Advent of Code website (inspect cookies in browser dev tools, copy the `session` value).

Set it as an environment variable:
```bash
export AOC_SESSION=your_session_cookie_here
```

Then fetch the input:
```bash
sbt "run load <day>"
```

For example:
```bash
sbt "run load 1"  # Downloads input to puzzle-inputs/day01/input.txt
```

### Run tests
```bash
sbt test
```
