package brainfuck

import brainfuck.Instruction._
import brainfuck.Machine.{Pointer, Register}
import cats.Monad
import cats.implicits._
import cats.mtl.MonadState

object Evaluator {

  def evaluate[F[_]: Monad: MonadState[?[_], Machine]: Console](program: Program): F[Unit] = {

    def eval(i: Instruction): F[Unit] = i match {
      case IncrementPointer                    => updatePointer(_.increment)
      case DecrementPointer                    => updatePointer(_.decrement)
      case IncrementCurrentRegister            => updateCurrentRegister(_.increment)
      case DecrementCurrentRegister            => updateCurrentRegister(_.decrement)
      case OutputCurrentRegister               => outputCurrentRegister
      case ReadAndSetCurrentRegister           => readAndSetCurrentRegister
      case DoWhileCurrentRegisterNonZero(loop) => doWhileCurrentRegisterNonZero(loop)
    }

    def updatePointer(f: Pointer => Pointer) =
      MonadState.modify[F, Machine](m => m.copy(pointer = f(m.pointer)))

    def updateCurrentRegister(f: Register => Register) =
      MonadState.modify[F, Machine](m => {
        val v = f(m.currentRegister)
        m.copy(registers = m.registers.updated(m.pointer.value, v))
      })

    def outputCurrentRegister =
      for {
        r <- currentRegister
        _ <- Console[F].writeByte(r.value)
      } yield ()

    def currentRegister = MonadState.get[F, Machine].map(_.currentRegister)

    def readAndSetCurrentRegister =
      for {
        v <- Console[F].readByte()
        _ <- updateCurrentRegister(_ => Register(v))
      } yield ()

    def doWhileCurrentRegisterNonZero(loop: Program) = {
      val currentRegisterNonZero = currentRegister.map(_.value != 0)
      Monad[F].whileM_(currentRegisterNonZero)(evaluate[F](loop))
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
}
