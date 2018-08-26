package brainfuck

import cats.Monad
import cats.data.StateT
import cats.effect.IO
import cats.mtl.MonadState
import cats.mtl.implicits._
import io.estatico.newtype.macros.newtype

object Main {

  def main(args: Array[String]): Unit = {
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
    val brainfuck = Evaluator.evaluate[Brainfuck](program)

    // TODO: Bump `cats-effect` and `.void` this
    brainfuck.runBrainfuck(machine).unsafeRunSync() // prints "Hello World!"
    ()
  }

  @newtype
  final case class Brainfuck[A](run: StateT[IO, Machine, A]) {
    def runBrainfuck(machine: Machine): IO[(Machine, A)] =
      run.run(machine)
  }

  object Brainfuck {

    implicit val monad: Monad[Brainfuck]                    = derivingK
    implicit val monadState: MonadState[Brainfuck, Machine] = derivingK[MonadState[?[_], Machine]]

    implicit val console: Console[Brainfuck] = new Console[Brainfuck] {
      override def writeByte(byte: Byte): Brainfuck[Unit] = Brainfuck(StateT.liftF(IO(print(byte.toChar))))
      override def readByte(): Brainfuck[Byte]            = Brainfuck(StateT.liftF(IO(scala.io.StdIn.readByte())))
    }
  }
}
