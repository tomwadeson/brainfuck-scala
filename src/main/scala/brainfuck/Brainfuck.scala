package brainfuck

import cats.Monad
import cats.data.StateT
import cats.effect.IO
import cats.mtl.MonadState
import cats.mtl.implicits._

sealed trait Instruction

object Instruction {
  case object IncrementPointer                           extends Instruction
  case object DecrementPointer                           extends Instruction
  case object IncrementValueAtPointer                    extends Instruction
  case object DecrementValueAtPointer                    extends Instruction
  case object OutputValueAtPointer                       extends Instruction
  case object ReadAndSetValueAtPointer                   extends Instruction
  case class DoWhileValueAtPointerNonZero(loop: Program) extends Instruction
}

final case class Program(value: List[Instruction]) extends AnyVal

final case class Brainfuck[A](run: StateT[IO, Machine, A]) extends AnyVal

object Brainfuck {

  type Stack[A] = StateT[IO, Machine, A]

  implicit object BrainfuckConsole extends Console[Brainfuck] {
    override def readByte(): Brainfuck[Byte]            = Brainfuck(StateT.lift[IO, Machine, Byte](IO(io.StdIn.readByte())))
    override def writeChar(char: Char): Brainfuck[Unit] = Brainfuck(StateT.lift[IO, Machine, Unit](IO(print(char))))
  }

  implicit val BrainfuckMonad: Monad[Brainfuck] = new Monad[Brainfuck] {

    override def flatMap[A, B](fa: Brainfuck[A])(f: A => Brainfuck[B]): Brainfuck[B] =
      Brainfuck(Monad[Stack].flatMap[A, B](fa.run)(f andThen (_.run)))

    override def tailRecM[A, B](a: A)(f: A => Brainfuck[Either[A, B]]): Brainfuck[B] =
      Brainfuck(Monad[Stack].tailRecM[A, B](a)(f andThen (_.run)))

    override def pure[A](x: A): Brainfuck[A] = Brainfuck(Monad[Stack].pure[A](x))
  }

  implicit val BrainfuckMonadState: MonadState[Brainfuck, Machine] = new MonadState[Brainfuck, Machine] {
    override val monad: Monad[Brainfuck] = BrainfuckMonad

    override def get: Brainfuck[Machine] = Brainfuck(MonadState.get[Stack, Machine])

    override def set(s: Machine): Brainfuck[Unit] = Brainfuck(MonadState.set[Stack, Machine](s))

    override def inspect[A](f: Machine => A): Brainfuck[A] = Brainfuck(MonadState.inspect[Stack, Machine, A](f))

    override def modify(f: Machine => Machine): Brainfuck[Unit] = Brainfuck(MonadState.modify[Stack, Machine](f))
  }
}
