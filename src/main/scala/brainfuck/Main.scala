package brainfuck

import cats.mtl.implicits._

object Main extends App {

  // A `Machine` with 20 registers
  val machine = Machine.sized(20)

  // Brainfuck sourcecode for a program which outputs "Hello World!"
  val sourcecode =
    """
      |++++++++++[>+++++++>++++++++++>+++>+<<<<-]
      |>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.
    """.stripMargin

  // Equivalent Brainfuck AST for the above sourcecode
  val program = Parser.parse(sourcecode).right.get // Don't do this!

  // A "Brainfuck computation", ready to run on a machine
  val brainfuck: Brainfuck[Unit] = Evaluator.evaluate[Brainfuck](program)

  brainfuck.run(machine).unsafeRunSync() // prints "Hello World!"
}
