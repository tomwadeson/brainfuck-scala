package brainfuck

import brainfuck.Instruction._
import brainfuck.Machine.{Pointer, Register}
import cats.Monad
import cats.data.StateT
import cats.effect.IO
import cats.implicits._
import cats.mtl.MonadState

object Evaluator {

  def evaluate[F[_]: Monad: MonadState[?[_], Machine]: Console](program: Program): F[Unit] = {

    def eval(i: Instruction): F[Unit] = i match {
      case IncrementPointer                   => incrementPointer()
      case DecrementPointer                   => decrementPointer()
      case IncrementValueAtPointer            => incrementValueAtPointer()
      case DecrementValueAtPointer            => decrementValueAtPointer()
      case OutputValueAtPointer               => outputValueAtPointer()
      case ReadAndSetValueAtPointer           => readAndSetValueAtPointer()
      case DoWhileValueAtPointerNonZero(loop) => doWhileValueAtPointerNonZero(loop)
    }

    def incrementPointer() = updatePointer(_.increment)

    def decrementPointer() = updatePointer(_.decrement)

    def updatePointer(f: Pointer => Pointer) =
      MonadState.modify[F, Machine](m => m.copy(pointer = f(m.pointer)))

    def incrementValueAtPointer() = updateRegisterAtPointer(_.increment)

    def decrementValueAtPointer() = updateRegisterAtPointer(_.decrement)

    def updateRegisterAtPointer(f: Register => Register) =
      MonadState.modify[F, Machine](m => {
        val v = f(m.currentRegister)
        m.copy(registers = m.registers.updated(m.pointer.value, v))
      })

    def outputValueAtPointer() =
      for {
        r <- getRegisterAtPointer()
        _ <- Console[F].writeByte(r.value)
      } yield ()

    def getRegisterAtPointer() = MonadState.get[F, Machine].map(_.currentRegister)

    def readAndSetValueAtPointer() =
      for {
        v <- Console[F].readByte()
        _ <- updateRegisterAtPointer(_ => Register(v))
      } yield ()

    def doWhileValueAtPointerNonZero(loop: Program) = {
      val valueAtPointerNonZero = getRegisterAtPointer().map(_.value != 0)
      Monad[F].whileM_(valueAtPointerNonZero)(loop.value.traverse_(eval))
    }

    program.value.traverse_(eval)
  }
}

final case class Machine(registers: Vector[Register], pointer: Pointer) {
  def currentRegister: Register = registers(pointer.value)
}

object Machine {

  final case class Register(value: Byte) extends AnyVal {
    def increment: Register = copy(value = (value + 1).toByte)
    def decrement: Register = copy(value = (value - 1).toByte)
  }

  final case class Pointer(value: Int) extends AnyVal {
    def increment: Pointer = copy(value = value + 1)
    def decrement: Pointer = copy(value = value - 1)
  }

  def sized(size: Int): Machine = Machine(Vector.fill(size)(Register(0)), Pointer(0))
}

trait Console[F[_]] {
  def readByte(): F[Byte]
  def writeByte(byte: Byte): F[Unit]
}

object Console {
  def apply[F[_]: Console]: Console[F] = implicitly

  implicit val brainfuckConsole: Console[Brainfuck] = new Console[Brainfuck] {
    override def writeByte(byte: Byte): Brainfuck[Unit] = StateT.lift(IO(print(byte.toChar)))
    override def readByte(): Brainfuck[Byte]            = StateT.lift(IO(io.StdIn.readByte()))
  }
}
